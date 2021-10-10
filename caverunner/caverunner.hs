#!/usr/bin/env stack 
{- stack script --optimize --verbosity=warn --resolver=lts-18.10
  --ghc-options=-threaded
  --package ansi-terminal
  --package ansi-terminal-game
  --package containers
  --package directory
  --package filepath
  --package linebreak
  --package pretty-simple
  --package process
  --package safe
  --package timers-tick
  --package unidecode
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

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-imports #-}
{-# LANGUAGE MultiWayIf, NamedFieldPuns, RecordWildCards, ScopedTypeVariables #-}

import Control.Applicative
import Control.Concurrent
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

progname  = "caverunner"
version   = "1.0alpha"
banner = unlines [
   "  _________ __   _____  _______  ______  ____  ___  _____"
  ," / ___/ __ `/ | / / _ \\/ ___/ / / / __ \\/ __ \\/ _ \\/ ___/"
  ,"/ /__/ /_/ /| |/ /  __/ /  / /_/ / / / / / / /  __/ /    "
  ,"\\___/\\__,_/ |___/\\___/_/   \\__,_/_/ /_/_/ /_/\\___/_/     "
  ]
usage w h = banner ++ unlines [
    ------------------------------------------------------------------------------80
   ""
  ,"caverunner "++version++" - a small terminal game by Simon Michael."
  ,"Thrillseeking drone pilots dive the solar system's caves, competing for glory."
  ,"Fly fast, avoid the walls!"
  ,""
  ,"Usage:"
  ,"$ ./caverunner.hs [ARGS]       # update the `caverunner` binary, then run it"
  ,"$ ./caverunner [CAVE [SPEED]]  # play, maybe changing cave (1) & max speed (15)"
  ,"$ ./caverunner -h|--help       # show this help"
  ,"$ ./caverunner --print-cave [CAVE [DEPTH]]  # show the full cave, or to DEPTH"
  ,""
  ,"Each CAVE has a high score (the max depth achieved) for each max speed."
  ,"80x25 terminals are best for competition play. Your current terminal is "++show w++"x"++show h++"."
  ,""
  ,"SPEED limits your maximum dive speed, 1-60 fathoms per second (default 15)."
  ,"High speeds make survival difficult, but increase the glory!"
  ,""
  ]
soundHelpDisabled = unlines [
   "To enable sound effects, install sox in PATH:"
  ,"apt install sox / brew install sox / choco install sox.portable / etc."
  ]
soundHelpEnabled soxpath = unlines [
   "Sound effects are enabled, using " ++ soxpath ++ ". --no-sound to disable."
  ]

-------------------------------------------------------------------------------

savefilename       = progname ++ ".save"
(leftkey,rightkey) = (',','.')
wallchar           = '#'
spacechar          = ' '
crashchar          = '*'
fps                = 60  -- target frame rate; computer/terminal may not achieve it
restartdelaysecs   = 5

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

cavespeedinit  = 1     -- initial cave vertical speed (player's speed within the cave, really)
cavespeedaccel = 1.01  -- multiply speed by this much each game tick (gravity)
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

type CaveNum    = Int    -- the number of a cave (and its random seed)
type MaxSpeed   = Int    -- a maximum dive/scroll speed in a cave
type Speed      = Float
type Score      = Integer
type HighScores = M.Map (CaveNum, MaxSpeed) Score

-- Coordinates within the on-screen game drawing area, from 1,1 at top left.
type GameRow = Row
type GameCol = Column

-- Coordinates within a cave, from 1,1 at the cave mouth's midpoint.
type CaveRow = Row
type CaveCol = Column

-- One line within a cave, with its left/right wall positions.
data CaveLine = CaveLine GameCol GameCol deriving (Show)

data GameState = GameState {
   gamew           :: Width      -- width of the game  (but perhaps not the screen)
  ,gameh           :: Height     -- height of the game (and usually the screen)
  ,gtick           :: Integer    -- current game tick
  ,highscore       :: Score      -- high score for the current cave and max speed
  ,score           :: Score      -- current score in this game
  ,cavenum         :: CaveNum
  ,randomgen       :: StdGen
  ,cavesteps       :: Integer    -- how many cave lines have been generated since game start
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
  ,gameover        :: Bool       -- player has crashed ?
  ,restarttimer    :: Timed Bool -- delay before restart after player crash
  ,pause           :: Bool       -- keep the game paused ?
  ,exit            :: Bool       -- completely exit the app ?
  }
  deriving (Show)

newGameState w h cavenum maxspeed hs = GameState {
   gamew           = w
  ,gameh           = h
  ,gtick           = 0
  ,highscore       = hs
  ,score           = 0
  ,cavenum         = cavenum
  ,randomgen       = mkStdGen cavenum
  ,cavesteps       = 0
  ,cavelines       = []
  ,cavewidth       = cavewidthinit
  ,cavecenter      = half w
  ,cavespeed       = cavespeedinit
  ,cavespeedmin    = cavespeedinit * 2
  ,cavespeedmax    = maxspeed
  ,cavetimer       = newCaveTimer cavespeedinit
  ,speedpan        = 0
  ,playery         = playerYMin h
  ,playerx         = half w
  ,playerchar      = 'V'
  ,gameover        = False
  ,restarttimer    = creaBoolTimer $ secsToTicks restartdelaysecs
  ,pause           = False
  ,exit            = False
  }

-------------------------------------------------------------------------------

main = do
  args <- getArgs
  when ("-h" `elem` args || "--help" `elem` args) $ exitWithUsage
  let
    defcavenum = 1
    defspeed   = 15
    (flags, args') = partition ("-" `isPrefixOf`) args
    (cavenum, speed) =
      case args' of
        []    -> (defcavenum, defspeed)
        [c]   -> (readDef (caveerr c) c, defspeed)
        [c,s] -> (readDef (caveerr c) c, readDef (speederr s) s)
        _     -> err "too many arguments, please see --help"
        where
          caveerr a = err $
            "CAVE should be a natural number (received "++a++"), see --help)"
          speederr a = err $
            "SPEED should be 1-60 (received "++a++"), see --help)"
  if 
    --  | "--print-speed-sound-volume" `elem` flags -> printSpeedSoundVolumes
    | "--print-cave" `elem` flags ->
      let mlimit = if speed==defspeed then Nothing else Just (round speed)
      in printCave cavenum mlimit
    | otherwise -> readHighScores >>= repeatGame cavenum speed

exitWithUsage = do
  clearScreen
  setCursorPosition 0 0
  (screenw,screenh) <- displaySize
  putStr $ usage screenw screenh
  msox <- findExecutable "sox"
  putStr $ case msox of
             Nothing  -> soundHelpDisabled
             Just sox -> soundHelpEnabled sox
  exitSuccess

-- Generate the cave just like the game would, printing each line to stdout.
-- Optionally, limit to just the first N lines.
printCave cavenum mlimit = do
  putStrLn $ progname ++ " cave "++show cavenum
  go mlimit $ newGameState gamewidth 25 cavenum 15 0
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

-- Play the game repeatedly, saving new high scores and/or
-- advancing to next cave when appropriate.
repeatGame :: CaveNum -> Speed -> HighScores -> IO ()
repeatGame cavenum maxspeed highscores = do
  when soundEnabled $ gameStartSound
  (_,screenh) <- displaySize  -- use full screen height for each game (apparently last line is unusable on windows ? surely it's fine)
  let
    highscore = fromMaybe 0 $ M.lookup (cavenum, round maxspeed) highscores
    game = newGame screenh cavenum maxspeed highscore
  g@GameState{score,exit} <- Terminal.Game.playGameS game
  let
    highscore'  = max score highscore
    highscores' = M.insert (cavenum, round maxspeed) highscore' highscores
    cavenum'    = if playerAtEnd g then cavenum+1 else cavenum
  when (highscore' > highscore) $ writeHighScores highscores'
  repeatGame cavenum' maxspeed highscores' & unless exit

-- Initialise a new game (a cave run).
newGame :: Height -> CaveNum -> Speed -> Score -> Game GameState
newGame gameh cavenum maxspeed hs =
  Game { gScreenWidth   = gamewidth, -- width used (and required) for drawing (a constant 80 for repeatable caves)
         gScreenHeight  = gameh,     -- height used for drawing (the screen height)
         gFPS           = fps,       -- target frames/game ticks per second
         gInitState     = newGameState gamewidth gameh cavenum maxspeed hs,
         gLogicFunction = step,
         gDrawFunction  = draw,
         gQuitFunction  = quit
       }

-------------------------------------------------------------------------------

-- Read high scores for each cave seed and speed from the save file.
readHighScores :: IO HighScores
readHighScores = do
  savefile <- saveFilePath
  exists <- doesFileExist savefile
  if exists
  then
    readDef (err $ "could not read high scores from\n"++savefile++"\nperhaps the format has changed, please move it out of the way")
    <$> readFile savefile
  else pure M.empty

writeHighScores highscores = do
  savefile <- saveFilePath
  createDirectoryIfMissing True $ takeDirectory savefile
  writeFile savefile $ show highscores

saveFilePath :: IO FilePath
saveFilePath = do
  datadir <- getXdgDirectory XdgData progname
  return $ datadir </> savefilename

-------------------------------------------------------------------------------

-- Handle input events.
step g@GameState{..} (KeyPress k)
  | k == 'q'              = g { exit = True }
  | k `elem` "p ", pause  = g { pause = False }
  | k `elem` "p "         = g { pause = True }
  | k == leftkey,  not (gameover || pause) =
      g { playerx = max 1 (playerx - 1)
        , cavespeed = max cavespeedmin (cavespeed * cavespeedbrake)
        }
  | k == rightkey, not (gameover || pause) =
      g { playerx = min gamew (playerx + 1)
        , cavespeed = max cavespeedmin (cavespeed * cavespeedbrake)
        }
  | otherwise = g

-- Handle a tick event (game heartbeat). Most updates to game state happen here.
step g@GameState{..} Tick =
  let
    g' = g{gtick     = gtick+1}
    gameover' = playerCrashed g
    victory   = playerAtEnd g
  in  -- breaks my haskell-mode's indentation
    if
      | pause ->  -- paused
        g'

      | not gameover && gameover' ->  -- newly crashed / reached the end
        unsafePlay (if victory then victorySound else crashSound cavespeed) $
        g'{gameover     = True
          ,highscore    = max score highscore
          ,restarttimer = reset restarttimer
          }

      | gameover ->  -- previously crashed, awaiting restart
        g'{restarttimer = tick restarttimer}

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
        in
          -- (if cavesteps `mod` 5 == 4 -- && cavespeed' > 5 
          --   then unsafePlay $ speedSound cavespeed' else id) $
          (if playerCloseShave g'    then unsafePlay closeShaveSound else id) $
          (if cavesteps `mod` 5 == 2 then unsafePlay $ depthSound cavesteps' else id) $
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
            ,gameover     = gameover'
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
        cannarrow = (cavesteps' `mod` interval) == 0
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
    cavelines' = take (int gameh * 2) $ CaveLine l r : cavelines
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
      length cavelines' >= int gameh
      && cavesteps' `mod` 5 == 0
    idealpan =
      round $
      float (playerYMax gameh - playerYMin gameh)
      * (cavespeed'-cavespeedinit) / (cavespeedmax-cavespeedinit)

-- increase score for every step deeper into the cave
stepScore g@GameState{..} cavesteps'
  | insidecave = score + 1
  | otherwise  = score
  where
    insidecave = cavesteps' > playerHeight g

-- Should the current game be ended ?
quit g@GameState{..}
  | exit = True     -- always end if q was pressed
  | pause = False   -- otherwise don't end if paused
  | gameover && isExpired restarttimer = True  --  end if the restart timer just ended
  | otherwise = False

-------------------------------------------------------------------------------

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
playerYMin gameh = round $ playerymin * float gameh
playerYMax gameh = round $ playerymax * float gameh

-- Player's current height above bottom of the game drawing area.
playerHeight :: GameState -> GameRow
playerHeight GameState{..} = gameh - playery

-- Player's current depth within the cave.
playerDepth :: GameState -> CaveRow
playerDepth g@GameState{..} = max 0 (cavesteps - playerHeight g)

-- The cave line currently at the player's position, if any.
playerLine :: GameState -> Maybe CaveLine
playerLine g@GameState{..} = cavelines `atMay` int (playerHeight g)

-- playerLineAbove g@GameState{..} = cavelines `atMay` int (playerHeight g - 1)

-- playerLineBelow g@GameState{..} = cavelines `atMay` int (playerHeight g + 1)

-- Has player hit a wall ?
playerCrashed g@GameState{..} =
  case playerLine g of
    Nothing             -> False
    Just (CaveLine l r) -> playerx <= l || playerx > r

-- Is player flying next to a wall ?
playerCloseShave g@GameState{..} =
  case playerLine g of
    Nothing             -> False
    Just (CaveLine l r) -> l == playerx-1 || r == playerx

-- Has player reached the cave bottom (a zero-width line) ?
playerAtEnd g = case playerLine g of
  Just (CaveLine l r) | r <= l -> True
  _                            -> False

-- Convert seconds to game ticks based on global frame rate.
secsToTicks :: Float -> Integer
secsToTicks = round . (* float fps)

half :: Integral a => a -> a
half = (`div` 2)

float :: Integer -> Float
float = fromInteger

int :: Integer -> Int
int = fromIntegral

err = errorWithoutStackTrace

-- -- Execute an IO action, using unsafePerformIO, before evaluating the
-- -- second argument.
-- unsafeio :: IO a -> b -> b
-- unsafeio = seq . unsafePerformIO

-- Execute an IO action, typically one that plays a sound asynchronously,
-- before evaluating the second argument, unless sound is disabled.
-- Uses unsafePerformIO.
unsafePlay :: IO a -> b -> b
unsafePlay a = if soundEnabled then seq (unsafePerformIO a) else id

-- Check whether sounds should be played - true unless the program was
-- run with the --no-sound flag. Uses unsafePerformIO. Won't change in a GHCI session.
-- {-# OPTIONS_GHC -fno-cse #-}
-- {-# NOINLINE soundEnabled #-}
soundEnabled :: Bool
soundEnabled = not $ "--no-sound" `elem` unsafePerformIO getArgs

-------------------------------------------------------------------------------

draw g@GameState{..} =
    blankPlane gamew gameh
  & (max 1 (gameh - toInteger (length cavelines) + 1), 1) % drawCave g
  -- & (1, 1)          % blankPlane gamew 1
  & (1, titlex)     % drawTitle g
  & (1, cavenamex)  % drawCaveName g
  & (if cavesteps < 25 || pause then (3, helpx) % drawHelp g else id)
  & (1, highscorex) % drawHighScore g
  & (1, scorex)     % drawScore g
  & (1, speedx)     % drawSpeed g
  -- & (3, gamew - 13) % drawStats g
  & (playery+speedpan, playerx) % drawPlayer g
  where
    titlew     = 12
    cavenamew  = fromIntegral $ 10 + length (show cavenum) + length (show cavespeedmax)
    highscorew = 17
    scorew     = 11
    speedw     = 10

    titlex     = 1
    helpx      = 1
    speedx     = gamew - speedw + 1
    scorex     = highscorex + highscorew + half (speedx - (highscorex + highscorew)) - half scorew
    highscorex = half gamew - half highscorew
    cavenamex  = min (highscorex - cavenamew) (half (highscorex - (titlex+titlew)) + titlex + titlew - half cavenamew)

drawCave GameState{..} =
  vcat $
  map (drawCaveLine gamew) $
  reverse $
  take (int gameh) $
  drop (int speedpan) cavelines

drawCaveLine gamew line = stringPlane $ showCaveLine gamew line

showCaveLine gamew (CaveLine left right) =
  concat [
    replicate (int left) wallchar
   ,replicate (int $ right - left) spacechar
   ,replicate (int $ gamew - right ) wallchar
   ]

showCaveLineWithNum gamew l n =
  num ++ drop (length num) (showCaveLine gamew l  )
  where
    num = show n

drawPlayer GameState{..} =
  cell char #bold #color hue Vivid
  where
    (char, hue) | gameover  = (crashchar,Red)
                | otherwise = (playerchar,Blue)

drawTitle GameState{..} =
  hcat $
  map bold $
  map (\(a,b) -> a b) $
  zip (drop (int cavesteps `div` 3 `mod` 4) $ cycle [color Red Vivid, color Green Vivid, color Blue Vivid, color Yellow Vivid]) $
  map cell (progname++"! ")

drawCaveName GameState{..} = stringPlane $ " cave "++show cavenum++" @ "++show (round cavespeedmax) ++ " "

drawHelp GameState{..} =
      (cell leftkey  #bold  ||| stringPlane " left ")
  === (cell rightkey #bold  ||| stringPlane " right ")
  === (cell 'p'      #bold  ||| if pause then stringPlane " pause " #bold else stringPlane " pause ")
  === (cell 'q'      #bold  ||| if exit then stringPlane " quit " #bold else stringPlane " quit ")

drawHighScore GameState{..} =
  stringPlane " high score " ||| (stringPlane (printf "%04d " highscore) & maybebold)
  where
    maybebold = if gameover && highscore==score then (#bold) else id

drawScore GameState{..} =
  stringPlane " score " ||| (stringPlane (printf "%04d " score) & maybebold)
  where
    maybebold = if score >= highscore then (#bold) else id

drawSpeed g@GameState{..} = stringPlane " speed " ||| stringPlane (printf "%3.f " cavespeed)

drawStats g@GameState{..} =
      (stringPlane "    depth " ||| stringPlane (printf "%3d " (playerDepth g)))
  === (stringPlane "    width " ||| stringPlane (printf "%3d " cavewidth))
  === (stringPlane " minspeed " ||| stringPlane (printf "%3.f " cavespeedmin))
  -- === (stringPlane " speedpan " ||| stringPlane (printf "%3d " speedpan))
  -- === (stringPlane "    speed " ||| stringPlane (printf "%3.f " cavespeed))

-------------------------------------------------------------------------------

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
playTones' tones = mapM_ playTone' tones

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
  let d = 0.1
  -- repeatTones 2 $ mkTones 100 $ [100,200,400,200]
  -- soxPlay False [show d,"sine","400-100"]
  -- threadDelay $ round $ d * 1000000
  soxPlay False [show d,"sine","400-100"]
  threadDelay $ round $ d * 1000000
  soxPlay False [show d,"sine","400-100"]
  threadDelay $ round $ d * 1000000
  soxPlay False [".5","sine","400-100"]

depthSound depth = do
  soxPlay False [".15", "sine", show $ 100 + depth, "vol .1"]
  soxPlay False [".15", "sine", show $ 1000 - depth, "vol .05"]
  return ()

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

closeShaveSound = do
  soxPlay False [".2 brownnoise", "fade p .2 -0 0", "vol .5"]

crashSound speed = do
  -- soxPlay False [l, "pinknoise",  "fade", "l", "0", "-0", l, "vol", v, "vol .2"]
  soxPlay False [d, "brownnoise", "fade", "l", "0", "-0", d, "vol 1"]
  soxPlay False [d, "brownnoise", "fade", "l", "0", "-0", d, "vol", v]
  where
    speed' = min 60 $ 30 + speed / 2
    d = show $ speed' / 6
    v = show $ crashSoundVolume speed'

crashSoundVolume s = s / 30

printCrashSoundVolumes = do
  putStrLn "        volume"
  putStrLn "speed       123456789"
  putStr $ unlines $ reverse $ [printf "%5.f  %4.1f " s v ++ replicate (round $ v * 10) '*' | (s,v) <- vols]
  where vols = [(s, crashSoundVolume s) | s <- [0,5..60::Speed]]

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

