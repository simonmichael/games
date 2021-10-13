#!/usr/bin/env stack
{- stack script --optimize --verbosity=warn --resolver=nightly-2021-10-12
  --ghc-options=-threaded
  --package ansi-terminal
  --package ansi-terminal-game
  --package containers
  --package directory
  --package filepath
  --package pretty-simple
  --package process
  --package safe
-}
-- stack (https://www.fpcomplete.com/haskell/get-started) is the easy
-- way to run this script reliably. On first run the script may seem
-- to hang (perhaps for minutes) while downloading and unpacking GHC;
-- change to --verbosity=info above to see more output.
--
-- You can also use cabal and/or your system package manager to
-- install the above haskell packages and a suitable GHC version (eg
-- 8.10), then compile the script.

-------------------------------------------------------------------------------
-- language & imports

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-imports #-}
{-# LANGUAGE MultiWayIf, NamedFieldPuns, RecordWildCards, ScopedTypeVariables #-}

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Function (fix)
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Debug.Trace
import Safe
import System.Console.ANSI
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Terminal.Game
import Text.Pretty.Simple (pPrint)
import Text.Printf

-------------------------------------------------------------------------------
-- help

progname  = "caverunner"
version   = "1.0alpha"
banner = unlines [
   "  _________ __   _____  _______  ______  ____  ___  _____"
  ," / ___/ __ `/ | / / _ \\/ ___/ / / / __ \\/ __ \\/ _ \\/ ___/"
  ,"/ /__/ /_/ /| |/ /  __/ /  / /_/ / / / / / / /  __/ /    "
  ,"\\___/\\__,_/ |___/\\___/_/   \\__,_/_/ /_/_/ /_/\\___/_/     "
  ]
usage termsize msoxpath = banner ++ unlines [
    ------------------------------------------------------------------------------80
   ""
  ,"caverunner "++version++" - a small terminal game by Simon Michael."
  ,"Thrillseeking drone pilots dive the solar system's caves, competing for glory."
  ,"Fly fast, avoid the walls!"
  ,""
  ,"Usage:"
  ,"$ ./caverunner.hs [ARGS]                               # compile & run the game"
  ,"$ ./caverunner [CAVE [SPEED]] [--stats] [--no-sound]   # run the game"
  ,"$ ./caverunner --print-cave [CAVE [DEPTH]]             # print the cave"
  ,"$ ./caverunner --help|-h"
  ,""
  ,"CAVE and SPEED select a different cave number and max speed (1-60, default 15)."
  ,"These are persistent. Higher speeds increase the glory!"
  ,""
  ,"80x25 terminals are best for competition play. Your current terminal is "++termsize++"."
  ,""
  ]
  ++ case msoxpath of
    Nothing -> unlines [
      "To enable sound effects, install sox in PATH:"
      ,"apt install sox / brew install sox / choco install sox.portable / etc."
      ]
    Just soxpath -> unlines [
      "Sound effects are enabled, using " ++ soxpath ++ ". --no-sound to disable."
      ]

-------------------------------------------------------------------------------
-- tweakable parameters

(leftkey,rightkey) = (',','.')
wallchar           = '#'
spacechar          = ' '
crashchar          = '*'
fps                = 60  -- target frame rate; computer/terminal may not achieve it
restartdelaysecs   = 3   -- minimum pause after game over

{- Three widths:

<-----------------------------screen width (terminal size)------------------------------->

    <----------------------------------game width (80)----------------------------->
    caverunner!    cave 1 @ 15      high score 0450        score 0000      speed   2  

    ####################<---cave width (decreasing from 40)---->####################
    #####################                                        ###################
    #####################                                      #####################

-}

-- parameters affecting cave procedural generation, keep these and stepCave the same for repeatable caves
gamewidth     = 80     -- how many columns to use (and require) for the game ? Can reduce it during development
cavemarginmin = 2      -- how close can cave walls get to the game edge ?
cavewidthinit = 40     -- how wide should the cave mouth be ?
cavewidthmin  = 0      -- how narrow can the cave get ?
cavewidthdurations = [ -- how long should the various cave widths last ?
-- (W,N): at width >= W, narrow (by 1) every N cave lines
   (20,3)
  ,(10,10)
  ,( 8,50)
  ,( 7,50)
  ,( 6,50)
  ,( 5,50)
  ,( 4,50)
  ,( 3,10)
  ,( 2,3)
  ]
-- Eg:
--  (20, 2) "at 20+,   narrow (by 1) every 2 lines"
--  (10,10) "at 10-19, narrow every 10 lines"
--  ( 8,50) "at 8-9,   narrow every 50 lines"

defcavenum  = 1
defmaxspeed = 15

cavespeedinit  = 1     -- initial cave vertical speed (player's speed within the cave, really)
cavespeedaccel = 1.008  -- multiply speed by this much each game tick (gravity)
cavespeedbrake = 1     -- multiply speed by this much each player movement (autobraking)

(playerymin, playerymax) = (0.4, 0.4)  -- player bounds relative to screen height, different bounds enables speed panning

-- cavespeedinit  = 2            
-- cavespeedaccel = 1.005         
-- cavespeedbrake = 0.7           
-- (playerymin, playerymax) = (0.2, 0.6)

-- cavespeedinit  = 4
-- cavespeedaccel = 1.001
-- cavespeedbrake = 0.8
-- (playerymin, playerymax) = (0.4, 0.4)

-------------------------------------------------------------------------------
-- types

type CaveNum    = Int    -- the number of a cave (and its random seed)
type MaxSpeed   = Int    -- a maximum dive/scroll speed in a cave
type Speed      = Float
type Score      = Int

-- Coordinates within the on-screen game drawing area, from 1,1 at top left.
type GameRow = Row
type GameCol = Column

-- Coordinates within a cave, from 1,1 at the cave mouth's midpoint.
type CaveRow = Row
type CaveCol = Column

-- One line within a cave, with its left/right wall positions.
data CaveLine = CaveLine GameCol GameCol deriving (Show)

data Scene =
    Playing
  | Crashed
  | Won
  deriving (Show,Eq)

data GameState = GameState {
  -- options
   stats           :: Bool       -- whether to show statistics
  -- current app state
  ,scene           :: Scene      -- current app/game mode
  ,pause           :: Bool       -- keep things paused ?
  ,restarttimer    :: Timed Bool -- delay after game over before allowing a restart keypress
  ,restart         :: Bool       -- time to restart after game over ?
  ,exit            :: Bool       -- time to exit the app ?
  -- current game state
  ,gamew           :: Width      -- width of the game  (but perhaps not the screen)
  ,gameh           :: Height     -- height of the game (and usually the screen)
  ,gtick           :: Integer    -- current game tick
  ,cavenum         :: CaveNum
  ,highscore       :: Score      -- high score for the current cave and max speed
  ,score           :: Score      -- current score in this game
  ,randomgen       :: StdGen
  ,cavesteps       :: Int        -- how many cave lines have been generated since game start (should be Integer but I can't be bothered)
  ,cavelines       :: [CaveLine] -- recent cave lines, for display; newest/bottom-most first
  ,cavewidth       :: Width      -- current cave width (inner space between the walls)
  ,cavecenter      :: GameCol    -- current x coordinate in game area of the cave's horizontal midpoint
  ,cavespeed       :: Speed      -- current speed of player dive/cave scroll in lines/s, must be <= fps
  ,cavespeedmin    :: Speed      -- current minimum speed player can brake to
  ,cavespeedmax    :: Speed      -- maximum speed player can accelerate to, must be <= fps
  ,cavetimer       :: Timed Bool -- delay before next cave scroll
  ,speedpan        :: Height     -- current number of rows to pan the viewport down, based on current speed
  ,playery         :: GameRow    -- player's y coordinate in game area
  ,playerx         :: GameCol    -- player's x coordinate in game area
  ,playerchar      :: Char
  }
  deriving (Show)

newGameState stats w h cavenum maxspeed hs = GameState {
   stats           = stats
  ,scene           = Playing
  ,pause           = False
  ,restart         = False
  ,exit            = False
  ,restarttimer    = creaBoolTimer $ secsToTicks restartdelaysecs
  ,gamew           = w
  ,gameh           = h
  ,gtick           = 0
  ,cavenum         = cavenum
  ,highscore       = hs
  ,score           = 0
  ,randomgen       = mkStdGen cavenum
  ,cavesteps       = 0
  ,cavelines       = []
  ,cavewidth       = cavewidthinit
  ,cavecenter      = half w
  ,cavespeed       = fromIntegral maxspeed / 6 -- cavespeedinit
  ,cavespeedmin    = cavespeedinit * 2
  ,cavespeedmax    = fromIntegral maxspeed
  ,cavetimer       = newCaveTimer cavespeedinit
  ,speedpan        = 0
  ,playery         = playerYMin h
  ,playerx         = half w
  ,playerchar      = 'V'
  }

-- save files, separated for robustness

-- miscellaneous saved state

data SavedState = SavedState {
   currentcave  :: CaveNum     -- the cave most recently played
  ,currentspeed :: MaxSpeed    -- the max speed most recently played
  ,highcave     :: CaveNum     -- the highest cave number reached/unlocked
} deriving (Read, Show, Eq)

newSavedState = SavedState{
   currentcave  = defcavenum
  ,currentspeed = defmaxspeed
  ,highcave     = defcavenum
}

statefilename   = "state"
loadState       = load statefilename
saveState       = maybeSave statefilename

-- high scores

type SavedScores = M.Map (CaveNum, MaxSpeed) Score  -- high scores achieved for each cave and max speed 

newSavedScores = M.empty

scoresfilename  = "scores"
loadScores      = load scoresfilename
saveScores      = maybeSave scoresfilename

-------------------------------------------------------------------------------
-- app logic

main = do
  args <- getArgs
  when ("-h" `elem` args || "--help" `elem` args) exitWithUsage
  sstate@SavedState{..} <- fromMaybe newSavedState <$> loadState
  sscores               <- fromMaybe newSavedScores <$> loadScores
  let
    (flags, args') = partition ("-" `isPrefixOf`) args
    flag = (`elem` flags)
    (cavenum, speed, hasspeedarg) =
      case args' of
        []    -> (currentcave, currentspeed, False)
        [c]   -> (readDef (caveerr c) c, currentspeed, False)
        [c,s] -> (readDef (caveerr c) c, readDef (speederr s) s, True)
        _     -> err "too many arguments, please see --help"
        where
          caveerr a = err $
            "CAVE should be a natural number (received "++a++"), see --help)"
          speederr a = err $
            "SPEED should be 1-60 (received "++a++"), see --help)"
  if
    --  | "--print-speed-sound-volume" `elem` flags -> printSpeedSoundVolumes

    | "--print-cave" `elem` flags ->
      let mdepth = if hasspeedarg then Just speed else Nothing
      in printCave cavenum mdepth

    | otherwise -> do
      let sstate' = sstate{ currentcave=cavenum, currentspeed=speed }
      playGames (flag "--stats") cavenum (fromIntegral speed) sstate' sscores

-- Generate the cave just like the game would, printing each line to stdout.
-- Optionally, limit to just the first N lines.
printCave cavenum mdepth = do
  putStrLn $ progname ++ " cave "++show cavenum
  go mdepth $ newGameState False gamewidth 25 cavenum 15 0
  where
    go (Just 0) _ = return ()
    go mremaining g@GameState{..} =
      when (cavewidth > 0) $ do
        let
          (cavesteps',
           cavewidth',
           randomgen',
           cavecenter',
           cavelines'@(l:_)) = stepCave g
          (cavespeed',
           cavespeedmin') = stepSpeed g
        putStrLn $ showCaveLineWithNum gamewidth l cavesteps'
        go (fmap (subtract 1) mremaining)
           g{randomgen    = randomgen'
            ,cavesteps    = cavesteps'
            ,cavelines    = cavelines'
            ,cavewidth    = cavewidth'
            ,cavecenter   = cavecenter'
            ,cavespeed    = cavespeed'
            ,cavespeedmin = cavespeedmin'
            }

-- Play the game repeatedly, optionally showing stats, at the given cave and speed,
-- updating the save file and/or advancing to next cave when appropriate.
playGames :: Bool -> CaveNum -> MaxSpeed -> SavedState -> SavedScores -> IO ()
playGames stats cavenum maxspeed sstate@SavedState{..} sscores = do
  (_,screenh) <- displaySize  -- use full screen height for each game (apparently last line is unusable on windows ? surely it's fine)
  let
    highscore = fromMaybe 0 $ M.lookup (cavenum, maxspeed) sscores
    game = newGame stats screenh cavenum maxspeed highscore

  g@GameState{score,exit} <- Terminal.Game.playGameS game  -- run one game, will fail if terminal is too small
  saveState sstate  -- if cave or speed are new (and game started), remember them

  let
    cavenum'    = if playerAtEnd g then cavenum+1 else cavenum
    highcave'   = max cavenum' highcave
    highscore'  = max score highscore
    sscores'    = M.insert (cavenum, maxspeed) highscore' sscores
    sstate' = SavedState{
       currentcave  = cavenum'
      ,currentspeed = maxspeed
      ,highcave     = highcave'
      }
  saveScores sscores'  -- if there's a new high score, save it
  playGames stats cavenum' maxspeed sstate' sscores' & unless exit

-- Initialise a new game (a cave run).
newGame :: Bool -> Height -> CaveNum -> MaxSpeed -> Score -> Game GameState
newGame stats gameh cavenum maxspeed hs =
  Game { gScreenWidth   = gamewidth, -- width used (and required) for drawing (a constant 80 for repeatable caves)
         gScreenHeight  = gameh,     -- height used for drawing (the screen height)
         gFPS           = fps,       -- target frames/game ticks per second
         gInitState     = newGameState stats gamewidth gameh cavenum maxspeed hs,
         gLogicFunction = step,
         gDrawFunction  = draw,
         gQuitFunction  = quit
       }

-------------------------------------------------------------------------------
-- event handlers & game logic for each scene

step g@GameState{scene=Playing, ..} (KeyPress k)
  | k == 'q'              = g { exit = True }
  | k `elem` "p ", pause  = g { pause = False }
  | k `elem` "p "         = g { pause = True }
  | not pause, k == leftkey =
      g { playerx   = max 1 (playerx - 1)
        , cavespeed = max cavespeedmin (cavespeed * cavespeedbrake)
        }
  | not pause, k == rightkey =
      g { playerx   = min gamew (playerx + 1)
        , cavespeed = max cavespeedmin (cavespeed * cavespeedbrake)
        }
  | otherwise = g

step g@GameState{scene=Playing, ..} Tick =
  let
    g' = g{gtick = gtick+1}
    gamestart = gtick==0
    gameover  = playerCrashed g
    victory   = playerAtEnd g
  in
    if
      | pause ->  -- paused
        g'

      | gamestart -> unsafePlay gameStartSound g'

      | gameover ->  -- crashed / reached the end
        unsafePlay (if victory then victorySound else crashSound cavespeed) $
        (if score > highscore then unsafePlay gameEndHighScoreSound else id) $
        g'{scene        = if victory then Won else Crashed
          ,highscore    = max score highscore
          ,restarttimer = reset restarttimer
          }

      | isExpired cavetimer ->  -- time to step the cave
        let
          (cavespeed',
           cavespeedmin') = stepSpeed g
          (cavesteps',
           cavewidth',
           randomgen',
           cavecenter',
           cavelines') = stepCave     g'
          speedpan'    = stepSpeedpan g' cavesteps' cavespeed' cavelines'
          score'       = stepScore    g' cavesteps'
          -- depth        = playerDepth g'
          walldist     = fromMaybe 9999 $ playerWallDistance g'
        in
          -- (if cavesteps `mod` 5 == 4 -- && cavespeed' > 5 
          --   then unsafePlay $ speedSound cavespeed' else id) $
          -- XXX trying very hard to start the beeps at cave mouth, no success
          -- (if depth>0 && (depth-1) `mod` 5 == 1 
          -- (if cavesteps'>playerHeight g' && cavesteps' `mod` 5 == 0
          (if cavesteps `mod` 5 == 2
            then unsafePlay $ depthSound cavesteps' else id) $
          (if walldist <= 2 then unsafePlay $ closeShaveSound walldist else id) $
          -- (if score <= highscore && score' > highscore then unsafePlay highScoreSound else id) $
          g'{randomgen    = randomgen'
            ,score        = score'
            ,speedpan     = speedpan'
            ,cavesteps    = cavesteps'
            ,cavelines    = cavelines'
            ,cavewidth    = cavewidth'
            ,cavecenter   = cavecenter'
            ,cavespeed    = cavespeed'
            ,cavespeedmin = cavespeedmin'
            ,cavetimer    = newCaveTimer cavespeed'
            }

      | otherwise ->  -- time is passing
        let
          (cavespeed',
           cavespeedmin') = stepSpeed g
        in
          g'{cavetimer    = tick cavetimer
            ,cavespeed    = cavespeed'
            ,cavespeedmin = cavespeedmin'
            }

step g@GameState{..} (KeyPress 'q') = g { exit = True }

step g@GameState{..} Tick | gameOver g = g{restarttimer = tick restarttimer}

step g@GameState{..} (KeyPress _) | gameOver g, isExpired restarttimer = g{restart=True}

step g@GameState{..} (KeyPress _) | gameOver g = g

step g@GameState{..} (KeyPress k)
  | k `elem` "p "         = g{pause=True}
  | k `elem` "p ", pause  = g{pause=False}
  | otherwise             = g

step GameState{..} Tick = error $ "No handler for " ++ show scene ++ " -> " ++ show Tick ++ " !"

-------------------------------------------------------------------------------
-- logic helpers

-- Should the current game be ended ?
quit g@GameState{..}
  | exit = True           -- yes if q was pressed
  | pause = False         -- no if paused
  | restart = True        -- yes if a key was pressed after game over
  | otherwise = False

stepSpeed :: GameState -> (Speed, Speed)
stepSpeed GameState{..} = (speed', minspeed')
  where
    -- gravity - gradually accelerate
    speed' = min cavespeedmax (cavespeed * cavespeedaccel)
    -- hurryup - slowly increase minimum speed ?
    -- minspeed' = cavespeedinit * 2 + float (cavesteps `div` 100)
    minspeed' = cavespeedmin

stepCave GameState{..} =
  (cavesteps'
  ,cavewidth'
  ,randomgen'
  ,cavecenter'
  ,cavelines'
  )
  where
    cavesteps' = cavesteps + 1

    -- narrowing - gradually narrow cave
    cavewidth'
      | cannarrow = max cavewidthmin (cavewidth - 1)
      | otherwise = cavewidth
      where
        cannarrow = cavesteps' `mod` interval == 0
          where
            interval = maybe 1 snd $ find ((<= cavewidth) . fst) cavewidthdurations

    -- morejagged - slowly increase max allowed sideways shift ?
    maxdx =
      -- min (cavewidth' `div` 4) (cavesteps' `div` 100 + 1)
      min (cavewidth' `div` 4) 1

    -- choose cave's next x position, with constraints:
    -- keep the walls within bounds
    -- keep it somewhat navigable
    --  (user can move sideways at ~5/s, don't allow long traverses faster than that)
    (randomdx, randomgen') = getRandom (-maxdx,maxdx) randomgen
    cavecenter' =
      let
        x = cavecenter + randomdx
        (l,r) = caveWalls x cavewidth'
        (cavemin,cavemax) = (cavemarginmin, gamew - cavemarginmin)
      in
        if | l < cavemin -> cavemin + half cavewidth'
           | r > cavemax -> cavemax - half cavewidth'
           | otherwise   -> x

    -- extend the cave, discarding old lines,
    -- except for an extra screenful that might be needed for speedpan
    cavelines' = take (gameh * 2) $ CaveLine l r : cavelines
      where
        (l,r) = caveWalls cavecenter' cavewidth'

-- speedpan - as speed increases, pan the viewport up (player and walls move down)
-- with constraints:
-- only after screen has filled with cave steps
-- pan gradually, at most one row every few cave steps
-- keep player within configured min/max Y bounds
stepSpeedpan GameState{..} cavesteps' cavespeed' cavelines'
  | speedpan < idealpan, readytopan = speedpan+1
  | speedpan > idealpan, readytopan = speedpan-1
  | otherwise                       = speedpan
  where
    readytopan =
      length cavelines' >= gameh
      && cavesteps' `mod` 5 == 0
    idealpan =
      round $
      fromIntegral (playerYMax gameh - playerYMin gameh)
      * (cavespeed'-cavespeedinit) / (cavespeedmax-cavespeedinit)

-- increase score for every step deeper into the cave
stepScore g@GameState{..} cavesteps'
  | insidecave = score + 1
  | otherwise  = score
  where
    insidecave = cavesteps' > playerHeight g

-------------------------------------------------------------------------------
-- game state helpers

gameOver GameState{scene} = scene `elem` [Crashed,Won]

-- Create a timer for the next cave step, given the desired steps/s.
-- The steps/s should be no greater than ticks/s (the frame rate),
-- and will be capped at that (creating a one-tick timer).
newCaveTimer stepspersec = creaBoolTimer ticks
  where
    ticks = max 1 (secsToTicks  $ 1 / stepspersec)

-- Calculate the cave's left and right wall coordinates from center and width.
caveWalls center width = (center - half width, center + half width)

-- Player's minimum and maximum y coordinate in the game drawing area.
playerYMin, playerYMax :: Height -> GameRow
playerYMin gameh = round $ playerymin * fromIntegral gameh
playerYMax gameh = round $ playerymax * fromIntegral gameh

-- Player's current height above bottom of the game drawing area.
playerHeight :: GameState -> GameRow
playerHeight GameState{..} = gameh - playery

-- Player's current depth within the cave.
playerDepth :: GameState -> CaveRow
playerDepth g@GameState{..} = max 0 (cavesteps - playerHeight g)

-- The cave line currently at the player's position, if any.
playerLine :: GameState -> Maybe CaveLine
playerLine g@GameState{..} = cavelines `atMay` playerHeight g

playerLineAbove g@GameState{..} = cavelines `atMay` (playerHeight g - 1)

playerLineBelow g@GameState{..} = cavelines `atMay` (playerHeight g + 1)

-- Has player hit a wall ?
playerCrashed g@GameState{..} =
  case playerLine g of
    Nothing             -> False
    Just (CaveLine l r) -> playerx <= l || playerx > r

-- How close is player flying to a wall ?
-- Actually, checks the line below (ahead) of the player, to sync better with sound effects.
playerWallDistance :: GameState -> Maybe Width
playerWallDistance g@GameState{..} =
  case playerLineBelow g of
    Nothing             -> Nothing
    Just (CaveLine l r) -> Just $ min (max 0 $ playerx-l) (max 0 $ r+1-playerx)

-- Has player reached the cave bottom (a zero-width line) ?
playerAtEnd g = case playerLine g of
  Just (CaveLine l r) | r <= l -> True
  _                            -> False

-------------------------------------------------------------------------------
-- save file helpers

getSaveDir :: IO FilePath
getSaveDir = getXdgDirectory XdgData progname

-- Try to read a value from the named save file, using read,
-- returning Nothing if the file does not exist,
-- or exit with an error message if read fails.
load :: Read a => FilePath -> IO (Maybe a)
load filename = do
  d <- getSaveDir
  let f = d </> filename
  exists <- doesFileExist f
  if not exists
  then pure Nothing
  else
    Just . readDef (err $ init $ unlines [
      "could not read " ++ f
      ,"Perhaps the format has changed ? Please move it out of the way and run again."
      ])
    <$> readFile f    

-- Write a value to the named save file, using show,
-- creating our save directory if it does not exist.
save :: Show a => FilePath -> a -> IO ()
save filename val = do
  d <- getSaveDir
  createDirectoryIfMissing True d
  let f = d </> filename
  writeFile f $ show val

-- Like save, but don't write if the save file exists and already contains the same value.
-- Returns true if it wrote.
maybeSave :: (Eq a, Read a, Show a) => FilePath -> a -> IO Bool
maybeSave filename val = do
  mold <- load filename
  case mold of
    Just old | old == val -> return False
    _                     -> save filename val >> return True

-------------------------------------------------------------------------------
-- utilities

exitWithUsage = do
  clearScreen
  setCursorPosition 0 0
  termsize <- displaySizeStrSafe
  msox <- findExecutable "sox"
  putStr $ usage termsize msox
  exitSuccess

displaySizeStrSafe = do
  (w,h) <- displaySize `catch` \(_::SomeException) -> return (0,0)  -- XXX not working
  return $ show w++"x"++show h

-- Convert seconds to game ticks based on global frame rate.
secsToTicks :: Float -> Integer
secsToTicks = round . (* fromIntegral fps)

half :: Integral a => a -> a
half = (`div` 2)

-- float :: Int -> Float
-- float = fromIntegral

-- int :: Integer -> Int
-- int = fromIntegral

err = errorWithoutStackTrace

-- -- Execute an IO action, using unsafePerformIO, before evaluating the
-- -- second argument.
-- unsafeio :: IO a -> b -> b
-- unsafeio = seq . unsafePerformIO

-- Check whether sounds should be played - true unless the program was
-- run with the --no-sound flag. Uses unsafePerformIO. Won't change in a GHCI session.
soundEnabled :: Bool
soundEnabled = "--no-sound" `notElem` unsafePerformIO getArgs

-- Execute an IO action, typically one that plays a sound asynchronously,
-- before evaluating the second argument, unless sound is disabled.
-- Uses unsafePerformIO.
unsafePlay :: IO a -> b -> b
unsafePlay a = if soundEnabled then seq (unsafePerformIO a) else id

-------------------------------------------------------------------------------
-- drawing for each scene

draw g@GameState{..} =
    blankPlane gamew gameh
  & (max 1 (gameh - length cavelines + 1), 1) % drawCave g
  -- & (1, 1)          % blankPlane gamew 1
  & (1, titlex)     % drawTitle g
  & (1, cavenamex)  % drawCaveName g
  & (if cavesteps < 25 || pause then (3, helpx) % drawHelp g else id)
  & (1, highscorex) % drawHighScore g
  & (1, scorex)     % drawScore g
  & (1, speedx)     % drawSpeed g
  & (playery+speedpan, playerx) % drawPlayer g
  & (if gameOver g then (gameovery, gameoverx) % drawGameOver g gameoverw gameoverh else id)
  & (if stats then (3, gamew - 13) % drawStats g else id)
  where
    titlew     = 12
    cavenamew  = fromIntegral $ 10 + length (show cavenum) + length (show cavespeedmax)
    highscorew = 17
    scorew     = 11
    speedw     = 10
    gameoverw  = 30
    gameoverh  = 7

    titlex     = 1
    helpx      = 1
    speedx     = gamew - speedw + 1
    scorex     = highscorex + highscorew + half (speedx - (highscorex + highscorew)) - half scorew
    highscorex = half gamew - half highscorew
    cavenamex  = min (highscorex - cavenamew) (half (highscorex - (titlex+titlew)) + titlex + titlew - half cavenamew)
    gameoverx  = half gamew - half gameoverw
    gameovery  = max (playery+2) $ half gameh - half gameoverh

-------------------------------------------------------------------------------
-- drawing helpers

drawGameOver GameState{..} w h =
  box w h '-'
  & (2,2) % box (w-2) (h-2) ' '
  & (txty,txtx) % stringPlane txt
  & (if isExpired restarttimer then (txty+2,txtx-7) % stringPlane "press a key to continue" else id)
  where
    innerw = w - 2
    innerh = h - 2
    txt = "GAME OVER"
    txtw = fromIntegral $ length txt
    txth = 2
    txtx = 2 + half innerw - half txtw
    txty = 3

drawCave GameState{..} =
  vcat $
  map (drawCaveLine gamew) $
  reverse $
  take gameh $
  drop speedpan cavelines

drawCaveLine gamew line = stringPlane $ showCaveLine gamew line

showCaveLine gamew (CaveLine left right) =
  concat [
    replicate left wallchar
   ,replicate (right - left) spacechar
   ,replicate (gamew - right ) wallchar
   ]

showCaveLineWithNum gamew l n =
  num ++ drop (length num) (showCaveLine gamew l  )
  where
    num = show n

drawPlayer GameState{..} =
  cell char #bold #color hue Vivid
  where
    (char, hue) | scene==Crashed = (crashchar,Red)
                | otherwise = (playerchar,Blue)

drawTitle GameState{..} =
  hcat $ zipWith (\a b -> a b) colors (map (bold.cell) $ progname++"! ")
  where
    colors =
      drop (cavesteps `div` 3 `mod` 4) $
      cycle [color Red Vivid, color Green Vivid, color Blue Vivid, color Yellow Vivid]

drawCaveName GameState{..} = stringPlane $ " cave "++show cavenum++" @ "++show (round cavespeedmax) ++ " "

drawHelp GameState{..} =
      (cell leftkey  #bold  ||| stringPlane " left ")
  === (cell rightkey #bold  ||| stringPlane " right ")
  === (cell 'p'      #bold  ||| if pause then stringPlane " pause " #bold else stringPlane " pause ")
  === (cell 'q'      #bold  ||| if exit then stringPlane " quit " #bold else stringPlane " quit ")

drawHighScore g@GameState{..} =
  stringPlane " high score " ||| (stringPlane (printf "%04d " highscore) & maybebold)
  where
    maybebold = if gameOver g && highscore==score then (#bold) else id

drawScore GameState{..} =
  stringPlane " score " ||| (stringPlane (printf "%04d " score) & maybebold)
  where
    maybebold = if score >= highscore then (#bold) else id

drawSpeed g@GameState{..} = stringPlane " speed " ||| stringPlane (printf "%3.f " cavespeed)

drawStats g@GameState{..} =
      (stringPlane "    depth " ||| stringPlane (printf "%3d " (playerDepth g)))
  -- === (stringPlane "depthmod5 " ||| stringPlane (printf "%3d " (playerDepth g `mod` 5)))
  === (stringPlane "    width " ||| stringPlane (printf "%3d " cavewidth))
  === (stringPlane " minspeed " ||| stringPlane (printf "%3.f " cavespeedmin))
  -- === (stringPlane " speedpan " ||| stringPlane (printf "%3d " speedpan))
  -- === (stringPlane "    speed " ||| stringPlane (printf "%3.f " cavespeed))

-------------------------------------------------------------------------------
-- sound

-- Play sounds with sox (http://sox.sourceforge.net), it it's in PATH.
-- Limitations:
-- - exit status is discarded; we don't know if sox is installed or succeeded
-- - there is a short gap between tones played in a sequence

-- A sound tone (usually a sine wave) with a given pitch and duration.
type Tone = (Hz, Ms)
type Hz = Float
type Ms = Int

-- Arguments to follow sox's `synth`, such as ["200","sine","1000"] (200hz sine wave for 1000ms).
type SynthArgs = [String]

-- Play a synthesised sound with sox if possible (if it is installed in PATH),
-- optionally blocking until the sound finishes playing, otherwise spawning
-- a thread to play it.
-- Limitations: there's a short delay before/after a sound, so sequences
-- have audible gaps between the sounds.
soxPlay :: Bool -> SynthArgs -> IO ()
soxPlay synchronous args = do
  msox <- findExecutable "sox" -- XXX not noticeably slow, but should cache
  case msox of
    Nothing  -> return ()
    Just sox ->
      (if synchronous then callCommand else void . spawnCommand) $
      sox ++ " -V0 -qnd synth " ++ unwords args

-- Like soxPlay, but plays a tone.
soxPlayTone :: Bool -> Tone -> IO ()
soxPlayTone synchronous (hz,ms) =
  soxPlay synchronous [show $ fromIntegral ms / 1000, "sine", show hz]

-- Play a tone (blocking).
playTone' :: Tone -> IO ()
playTone' = soxPlayTone True

-- Play a sequence of tones (blocking).
playTones' :: [Tone] -> IO ()
playTones' = mapM_ playTone'

-- Play a sequence of tones N times (blocking).
repeatTones' :: Int -> [Tone] -> IO ()
repeatTones' n tones = playTones' $ concat $ replicate n tones


-- Play a tone (non-blocking).
playTone :: Tone -> IO ()
playTone = soxPlayTone False

-- Play a sequence of tones (non-blocking).
playTones :: [Tone] -> IO ()
playTones tones = void $ forkIO $ mapM_ playTone' tones

-- Play a sequence of tones N times (non-blocking).
repeatTones :: Int -> [Tone] -> IO ()
repeatTones n tones = playTones $ concat $ replicate n tones


-- Generate a sequence of same-duration tones from a duration and a list of frequencies.
mkTones :: Ms -> [Hz] -> [Tone]
mkTones t freqs = [(f,t) | f <- freqs]


-- Sound effects. These mostly play sound(s) asynchronously, returning immediately.

gameStartSound = void $ forkIO $ do
  let d = 0.12
      v = 0.7
  soxPlay False [show d,"sine","400-100","vol",show v]
  threadDelay $ round $ d * 1000000
  soxPlay False [show d,"sine","400-100","vol",show v]
  threadDelay $ round $ d * 1000000
  soxPlay False [".5","sine","400-100","vol",show v]

depthSound depth = do
  let v = 0.3
  soxPlay False [".15", "sine", show $ 100 + depth, "vol", show v]
  soxPlay False [".15", "sine", show $ 1000 - depth, "vol", show $ v/3]

-- trying to mimic a variable constant hiss with short sounds - too fragile
-- speedSound speed = do
--   let d = 1 / speed
--   soxPlay False [
--     show d, "pinknoise",
--     "fade q", show $ d / 2, "0", show $ d / 2,
--     "vol", show $ speedSoundVolume speed
--     ]

-- speedSoundVolume :: Speed -> Float
-- speedSoundVolume speed = max 0 $ (speed - 10) / 200

-- printSpeedSoundVolumes = do
--   putStrLn "        volume"
--   putStrLn "speed       123456789"
--   putStr $ unlines $ reverse $ [printf "%5.f  %4.1f " s v ++ replicate (round $ v * 10) '*' | (s,v) <- vols]
--   where vols = [(s, speedSoundVolume s) | s <- [0,5..60::Speed]]

closeShaveSound distance = do
  let
    d = 0.2
    v | distance<=1 = 0.4
      | distance<=2 = 0.1
      | otherwise   = 0
  soxPlay False [
    show d, "brownnoise",
    "fade p .2 -0 0",
    "vol", show v
    ]

crashSound speed = do
  -- soxPlay False [l, "pinknoise",  "fade", "l", "0", "-0", l, "vol", v, "vol .2"]
  soxPlay False [d, "brownnoise", "fade", "l", "0", "-0", d, "vol 1"]
  soxPlay False [d, "brownnoise", "fade", "l", "0", "-0", d, "vol", v]
  where
    speed' = min 60 $ 30 + speed / 2
    d = show $ speed' / 6
    v = show $ crashSoundVolume speed'

crashSoundVolume s = s / 25

printCrashSoundVolumes = do
  putStrLn "        volume"
  putStrLn "speed       123456789"
  putStr $ unlines $ reverse $ [printf "%5.f  %4.1f " s v ++ replicate (round $ v * 10) '*' | (s,v) <- vols]
  where vols = [(s, crashSoundVolume s) | s <- [0,5..60::Speed]]

highScoreSound = soxPlay False [".1 sine 800 sine 800 delay 0 +.2"]

gameEndHighScoreSound = soxPlay False [".05 sine 400 sine 500 sine 600 sine 800 sine 800 delay "++delay++" +.1 +.1 +.1 +.2"] where delay = show $ restartdelaysecs / 2

victorySound = do
  playTone (200,100)
  playTone (300,100)
  playTone (400,100)
  threadDelay 160000
  playTone (200,1500)
  playTone (300,1500)
  playTone (400,1500)

-- soxPlay False [".3","sine","200-100"]
-- soxPlay False [".8","sine","300-1"]

