#!/usr/bin/env stack 
-- stack --resolver=lts-18 script --optimize --verbosity=warn --ghc-options=-threaded --package ansi-terminal --package ansi-terminal-game --package linebreak --package timers-tick --package unidecode --package safe --package containers --package directory --package filepath --package silently
--
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
import Control.Monad
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
import System.IO.Silently
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Terminal.Game
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
  ,"$ ./caverunner[.hs] ...        # play [& update the caverunner binary]"
  ,"$ ./caverunner [CAVE [SPEED]]  # play, maybe changing cave (1) & max speed (15)"
  ,"$ ./caverunner -h|--help       # show this help"
  ,""
  ,"Each CAVE has a high score (the max depth achieved) for each max speed."
  ,"Terminals <80 wide (and perhaps >80) are not guaranteed to make the same cave."
  ,"80x25 is preferred by competition pilots. Your current terminal is "++show w++"x"++show h++"."
  ,""
  ,"SPEED limits your maximum dive speed, 1-60 fathoms per second (default 15)."
  ,"High speeds make survival difficult, but increase the glory!"
  ,""
  ,"https://github.com/vareille/toot (and maybe sox) in PATH enables basic sounds."
  ]

-- Keyboard repeat rate configuration tips:
-- Mac, Big Sur:
-- https://apple.stackexchange.com/questions/411531/increase-keyrepeat-speed-in-macos-big-sur
--  defaults write NSGlobalDomain InitialKeyRepeat -int 10  # 15
--  defaults write NSGlobalDomain KeyRepeat -int 1          # 2
--  reboot

savefilename       = progname ++ ".save"
(leftkey,rightkey) = (',','.')
wallchar           = '#'
pathchar           = ' '
crashchar          = '*'
fps                = 60  -- target frame rate; computer/terminal may not achieve it
restartdelaysecs   = 5

pathmarginmin      = 2
pathwidthmin       = 0
-- how long to stay at each path width, eg:
--  (20, 2) "at 20+,   narrow (by 1) every 2 path steps"
--  (10,10) "at 10-19, narrow every 10 steps"
--  ( 8,50) "at 8-9,   narrow every 50 steps"
pathwidthdurations = [
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

pathspeedinit  = 1     -- initial path scrolling speed
pathspeedaccel = 1.01  -- multiply speed by this much each game tick (gravity)
pathspeedbrake = 1     -- multiply speed by this much each player movement (autobraking)
(playerymin, playerymax) = (0.4, 0.4)  -- player bounds relative to screen height, different bounds enables speed panning

-- pathspeedinit  = 2            
-- pathspeedaccel = 1.005         
-- pathspeedbrake = 0.7           
-- (playerymin, playerymax) = (0.2, 0.6)

-- pathspeedinit  = 4
-- pathspeedaccel = 1.001
-- pathspeedbrake = 0.8
-- (playerymin, playerymax) = (0.4, 0.4)

-------------------------------------------------------------------------------

type Cave       = Int
type MaxSpeed   = Int
type Speed      = Float
type Score      = Integer
type HighScores = M.Map (Cave,MaxSpeed) Score

data GameState = GameState {
   screenw         :: Width
  ,screenh         :: Height
  ,cave            :: Cave
  ,randomgen       :: StdGen
  ,gtick           :: Integer    -- current game tick
  ,highscore       :: Score      -- high score for the current cave and max speed
  ,score           :: Score      -- current score in this game
  ,speedpan        :: Height     -- current number of rows to pan the viewport down, based on current speed
  ,pathsteps       :: Integer    -- how many path segments have been traversed since game start
  ,path            :: [PathLine] -- recent path segments for display, newest/bottom-most first
  ,pathwidth       :: Width      -- current path width
  ,pathcenter      :: Column     -- current path center
  ,pathspeed       :: Speed      -- current speed of path scroll in steps/s, must be <= fps
  ,pathspeedmin    :: Speed      -- current minimum speed player can brake to
  ,pathspeedmax    :: Speed      -- maximum speed player can accelerate to (an integer), must be <= fps
  ,pathtimer       :: Timed Bool -- delay before next path scroll
  ,playery         :: Row
  ,playerx         :: Column
  ,playerchar      :: Char
  ,playercollision :: Bool       -- player has crashed ?
  ,restarttimer    :: Timed Bool -- delay before restart after player crash
  ,pause           :: Bool       -- keep the game paused ?
  ,exit            :: Bool       -- completely exit the app ?
  }

newGameState w h cave rg hs maxspeed = GameState {
   screenw         = w
  ,screenh         = h
  ,cave            = cave
  ,randomgen       = rg
  ,gtick           = 0
  ,highscore       = hs
  ,score           = 0
  ,speedpan        = 0
  ,pathsteps       = 0
  ,path            = []
  ,pathwidth       = min 40 $ half w
  ,pathcenter      = half w
  ,pathspeed       = pathspeedinit
  ,pathspeedmin    = pathspeedinit * 2
  ,pathspeedmax    = maxspeed
  ,pathtimer       = newPathTimer pathspeedinit
  ,playery         = playerYMin h
  ,playerx         = half w
  ,playerchar      = 'V'
  ,playercollision = False
  ,restarttimer    = creaBoolTimer $ secsToTicks restartdelaysecs
  ,pause           = False
  ,exit            = False
  }

data PathLine = PathLine Column Column  -- left wall, right wall


-------------------------------------------------------------------------------

main = do
  (w,h) <- displaySize
  highscores <- readHighScores
  args <- getArgs
  when ("-h" `elem` args || "--help" `elem` args) $ exitWithUsage w h
  let
    defcave  = 1
    defspeed = 15
    (cave, speed) =
      case args of
        []    -> (defcave, defspeed)
        [c]   -> (readDef (caveerr c) c, defspeed)
        [c,s] -> (readDef (caveerr c) c, readDef (speederr s) s)
        _     -> err "too many arguments, please see --help"
        where
          caveerr a = err $
            "CAVE should be a natural number (received "++a++"), see --help)"
          speederr a = err $
            "SPEED should be 1-60 (received "++a++"), see --help)"
  playloop w h highscores cave speed

exitWithUsage w h = do
  clearScreen
  setCursorPosition 0 0
  putStr $ usage w h
  exitSuccess  

playloop :: Width -> Height -> HighScores -> Cave -> Float -> IO ()
playloop w h highscores caveseed maxspeed = do
  let
    randomgen = mkStdGen caveseed
    highscore = fromMaybe 0 $ M.lookup (caveseed, round maxspeed) highscores
    t = 100
  repeatTones' 2 100 [100,200,400]
  GameState{score,exit} <- playGameS $ newGame w h caveseed randomgen highscore maxspeed
  let
    highscore'  = max score highscore
    highscores' = M.insert (caveseed, round maxspeed) highscore' highscores
  when (highscore' > highscore) $ writeHighScores highscores'
  unless exit $ do
    (w',h') <- displaySize
    playloop w' h' highscores' caveseed maxspeed

-- a high score for each cave seed is stored in the save file
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

newGame screenw screenh cave rg hs maxspeed =
  Game { gScreenWidth   = screenw,
         gScreenHeight  = screenh-1,  -- last line is unusable on windows apparently
         gFPS           = fps,
         gInitState     = newGameState screenw screenh cave rg hs maxspeed,
         gLogicFunction = step,
         gDrawFunction  = draw,
         gQuitFunction  = quit
       }

-------------------------------------------------------------------------------

step g@GameState{..} (KeyPress k)
  | k == 'q'              = g { exit = True }
  | k `elem` "p ", pause  = g { pause = False }
  | k `elem` "p "         = g { pause = True }
  | k == leftkey,  not (playercollision || pause) =
      g { playerx = max 1 (playerx - 1)
        , pathspeed = max pathspeedmin (pathspeed * pathspeedbrake)
        }
  | k == rightkey, not (playercollision || pause) =
      g { playerx = min screenw (playerx + 1)
        , pathspeed = max pathspeedmin (pathspeed * pathspeedbrake)
        }
  | otherwise = g

step g@GameState{..} Tick =
  let
    -- gravity - gradually accelerate
    pathspeed' = min pathspeedmax (pathspeed * pathspeedaccel)

    g' = g{gtick     = gtick+1
          ,pathspeed = pathspeed'
          }

    -- has player crashed ?
    playercollision' =
      case path `atMay` int (playerHeight g - 1) of
        Nothing             -> False
        Just (PathLine l r) -> playerx <= l || playerx > r

  in  -- breaks my haskell-mode's indentation
    if
      | pause ->  -- paused
        g'

      | not playercollision && playercollision' ->  -- newly crashed
        unsafePerformIO (playTone (100,1000)) `seq` -- XXX sound plays before crash is drawn
        g'{playercollision = True
          ,highscore       = max score highscore
          ,restarttimer    = reset restarttimer
          }

      | playercollision ->  -- previously crashed, awaiting restart
        g'{restarttimer = tick restarttimer}

      | isExpired pathtimer ->  -- time to step the path
        let
          (pathsteps',
           pathspeedmin',
           pathwidth',
           randomgen',
           pathcenter',
           path')   = stepPath     g'
          speedpan' = stepSpeedpan g' pathsteps' pathspeed' path'
          score'    = stepScore    g' pathsteps'
        in
          (if pathsteps' `mod` 100 == 0 then (unsafePerformIO (playTone (400,70)) `seq`) else id) $
          g'{randomgen       = randomgen'
            ,score           = score'
            ,speedpan        = speedpan'
            ,pathsteps       = pathsteps'
            ,path            = path'
            ,pathwidth       = pathwidth'
            ,pathcenter      = pathcenter'
            ,pathspeed       = pathspeed'
            ,pathspeedmin    = pathspeedmin'
            ,pathtimer       = newPathTimer pathspeed'
            ,playercollision = playercollision'
            }

      | otherwise ->  -- time is passing
        g'{pathtimer = tick pathtimer}

stepPath GameState{..} =
  (pathsteps'
  ,pathspeedmin'
  ,pathwidth'
  ,randomgen'
  ,pathcenter'
  ,path')
  where
    pathsteps' = pathsteps + 1

    -- hurryup - slowly increase minimum speed ?
    -- pathspeedmin' = pathspeedinit * 2 + float (pathsteps `div` 100)
    pathspeedmin' = pathspeedmin

    -- narrowing - gradually narrow path
    pathwidth'
      | cannarrow = max pathwidthmin (pathwidth - 1)
      | otherwise = pathwidth
      where
        cannarrow = (pathsteps' `mod` interval) == 0
          where
            interval = maybe 1 snd $ find ((<= pathwidth) . fst) pathwidthdurations

    -- morejagged - slowly increase max allowed sideways shift ?
    maxdx =
      -- min (pathwidth' `div` 4) (pathsteps' `div` 100 + 1)
      min (pathwidth' `div` 4) 1

    -- choose path's next x position, with constraints:
    -- keep the walls within bounds
    (randomdx, randomgen') = getRandom (-maxdx,maxdx) randomgen
    pathcenter' =
      let
        x = pathcenter + randomdx
        (l,r) = pathWalls x pathwidth'
        (pathmin,pathmax) = (margin, screenw - margin)
          where
            margin = max pathmarginmin (screenw `div` 40)
      in
        if | l < pathmin -> pathmin + half pathwidth'
           | r > pathmax -> pathmax - half pathwidth'
           | otherwise   -> x

    -- extend the path, discarding old lines,
    -- except for an extra screenful that might be needed for speedpan
    path' = take (int screenh * 2) $ PathLine l r : path
      where
        (l,r) = pathWalls pathcenter' pathwidth'

-- speedpan - as speed increases, pan the viewport up (player and walls move down)
-- with constraints:
-- only after screen has filled with path steps
-- pan gradually, at most one row every few path steps
-- keep player within configured min/max Y bounds
stepSpeedpan GameState{..} pathsteps' pathspeed' path'
  | speedpan < idealpan, readytopan = speedpan+1
  | speedpan > idealpan, readytopan = speedpan-1
  | otherwise                       = speedpan
  where
    readytopan = 
      length path' >= int screenh
      && pathsteps' `mod` 5 == 0
    idealpan =
      round $
      float (playerYMax screenh - playerYMin screenh)
      * (pathspeed'-pathspeedinit) / (pathspeedmax-pathspeedinit)

-- increase score for every step deeper into the cave
stepScore g@GameState{..} pathsteps'
  | pathsteps' >= playerHeight g = score + 1
  | otherwise                    = score

-- bot player
    -- skill = 0
    -- playerdx <- randomRIO $
    -- playerdx = fst $ -- XXX
    --   if | playerx < pathcenter' ->
    --          case skill of
    --            0 -> (-1,1)
    --            1 -> (0,1)
    --            _ -> (1,1)
    --      | playerx > pathcenter' ->
    --          case skill of
    --            0 -> (-1,1)
    --            1 -> (-1,0)
    --            _ -> (-1,-1)
    --      | otherwise ->
    --          case skill of
    --            0 -> (-1,1)
    --            1 -> (-1,1)
    --            _ -> (0,0)
    -- playerx' = playerx + playerdx

-- Should the current game be ended ?
quit g@GameState{..} =
  playercollision && isExpired restarttimer && not pause  --  if the restart timer just ended and not paused
  || exit  -- or if q was pressed

-------------------------------------------------------------------------------

draw g@GameState{..} =
    blankPlane screenw screenh
  & (max 1 (screenh - toInteger (length path)), 1) % drawPath g
  & (1, titlex) % drawTitle
  & (1, cavex) % drawCave g
  & (3, helpx) % drawHelp g
  & (1, highscorex) % drawHighScore highscore
  & (1, scorex) % drawScore score
  & (2, scorex) % drawSpeed g
  -- & (3, screenw - 13) % drawStats g
  & (playery+speedpan, playerx) % drawPlayer g
  where
    titlew     = 12
    cavew      = fromIntegral $ 10 + length (show cave) + length (show pathspeedmax)
    highscorew = 17
    scorew     = 11

    titlex     = 1
    helpx      = 1
    scorex     = screenw - scorew + 1
    highscorex = min (scorex - highscorew) (3 * screenw `div` 4 - highscorew)
    cavex      = min (highscorex - cavew) (screenw `div` 4)

drawPlayer GameState{..} =
  cell char #bold #color hue Vivid
  where
    (char, hue) | playercollision = (crashchar,Red)
                | otherwise       = (playerchar,Blue)

drawTitle = hcat [
   cell 'c' #bold #color Red Vivid
  ,cell 'a' #bold #color Blue Vivid
  ,cell 'v' #bold #color Yellow Vivid
  ,cell 'e' #bold #color Green Vivid
  ,cell 'r' #bold #color Red Vivid
  ,cell 'u' #bold #color Blue Vivid
  ,cell 'n' #bold #color Yellow Vivid
  ,cell 'n' #bold #color Green Vivid
  ,cell 'e' #bold #color Red Vivid
  ,cell 'r' #bold #color Blue Vivid
  ,cell '!' #bold #color Yellow Vivid
  ,cell ' '
  ]

drawCave GameState{..} = stringPlane $ " cave "++show cave++" @ "++show (round pathspeedmax) ++ " "

drawHelp GameState{..} =
      (cell leftkey  #bold  ||| stringPlane " left ")
  === (cell rightkey #bold  ||| stringPlane " right ")
  === (cell 'p'      #bold  ||| if pause then stringPlane " pause " #bold else stringPlane " pause ")
  === (cell 'q'      #bold  ||| if exit then stringPlane " quit " #bold else stringPlane " quit ")

drawHighScore score = stringPlane " high score " ||| stringPlane (printf "%04d " score) #bold

drawScore score = stringPlane " score " ||| stringPlane (printf "%04d " score) #bold

drawSpeed g@GameState{..} = stringPlane " speed " ||| stringPlane (printf "%4.f " pathspeed)

drawStats g@GameState{..} =
      (stringPlane "    depth " ||| stringPlane (printf "%3d " (max 0 (pathsteps - playerHeight g))))
  === (stringPlane "    width " ||| stringPlane (printf "%3d " pathwidth))
  === (stringPlane " minspeed " ||| stringPlane (printf "%3.f " pathspeedmin))
  -- === (stringPlane " speedpan " ||| stringPlane (printf "%3d " speedpan))
  -- === (stringPlane "    speed " ||| stringPlane (printf "%3.f " pathspeed))

drawPath GameState{..} = vcat (map (drawPathLine screenw) $ reverse $ take (int screenh) $ drop (int speedpan) path)

drawPathLine screenw (PathLine left right) = stringPlane line
  where
    line = concat [
       replicate (int left) wallchar
      ,replicate (int $ right - left) pathchar
      ,replicate (int $ screenw - right ) wallchar
      ]

-------------------------------------------------------------------------------

-- A sound tone (usually a sine wave) with a given pitch and duration.
type Tone = (Hz, Ms)
type Hz = Float
type Ms = Int

-- Tone playing functions, blocking.

-- Play a tone using toot (https://github.com/vareille/toot), if it's
-- found in PATH, returning its exit code. When toot is not in PATH,
-- return ExitFailure 1.
-- Limitations:
-- - This blocks while the sound is playing.
-- - On some systems, toot won't sound right unless sox is also installed.
-- - sox generates stderr output, which we suppress.
-- - There is a mastodon client also named toot, which won't play tones.
-- - Tone sequences are played with a gap between tones.
playTone :: Tone -> IO ExitCode
playTone (hz,ms) = do
  mtootapp <- findExecutable "toot.app"
  mtoot    <- findExecutable "toot"
  case mtootapp <|> mtoot of
    Just toot ->
      hSilence [stderr] $
      system $ toot ++ " -f " ++ show hz ++ " -l " ++ show ms
    Nothing   -> return $ ExitFailure 1

-- Play a sequence of tones.
-- This and the other multi-tone functions return the first non-zero exit code,
-- or ExitSuccess.
playTones :: [Tone] -> IO ExitCode
playTones tones = do
  codes <- mapM playTone tones
  let code = fromMaybe ExitSuccess $ headMay $ filter (/= ExitSuccess) codes
  return code

-- Play a sequence of tones with the same duration.
playTones' :: Ms -> [Hz] -> IO ExitCode
playTones' t freqs = playTones [(f,t) | f <- freqs]

-- Play a sequence of tones, N times.
repeatTones :: Int -> [Tone] -> IO ExitCode
repeatTones n tones = playTones $ concat $ replicate n tones

-- Play a sequence of tones with the same duration, N times.
repeatTones' :: Int -> Ms -> [Hz] -> IO ExitCode
repeatTones' n t freqs = playTones' t $ concat $ replicate n freqs


-------------------------------------------------------------------------------

-- Convert seconds to game ticks based on global frame rate.
secsToTicks :: Float -> Integer
secsToTicks = round . (* float fps)

-- Convert steps/s to ticks/step and create a timer for one step.
-- The steps/s should be no greater than ticks/s (the frame rate),
-- and will be capped at that (creating a one-tick timer).
newPathTimer stepspersec = creaBoolTimer ticks
  where
    ticks = max 1 (secsToTicks  $ 1 / stepspersec)

-- Convert player's y coordinate measured from screen top, to height measured from screen bottom.
playerHeight GameState{..} = screenh - playery

-- Calculate the player's minimum and maximum y coordinate.
playerYMin screenh = round $ playerymin * float screenh
playerYMax screenh = round $ playerymax * float screenh

-- Calculate the path's left and right wall coordinates from center and width.
pathWalls center width = (center - half width, center + half width)

half :: Integral a => a -> a
half = (`div` 2)

float :: Integer -> Float
float = fromInteger

int :: Integer -> Int
int = fromIntegral

err = errorWithoutStackTrace
