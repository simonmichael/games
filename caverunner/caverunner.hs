#!/usr/bin/env stack
{- stack script --optimize --verbosity=warn --resolver=nightly-2021-10-19
  --ghc-options=-threaded
  --package ansi-terminal
  --package ansi-terminal-game
  --package containers
  --package directory
  --package filepath
  --package pretty-show
  --package pretty-simple
  --package safe
  --package time
  --package typed-process
-}
-- stack (https://www.fpcomplete.com/haskell/get-started) is the easiest
-- way to run this program reliably. On first run the script may seem
-- to hang (perhaps for minutes) while downloading and unpacking GHC;
-- change to --verbosity=info above to see more output.
--
-- You can also use cabal and/or your system package manager to install
-- the above haskell packages and a suitable GHC version (eg 8.10),
-- and then compile the script.

-------------------------------------------------------------------------------
-- language & imports

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-imports #-}
{-# LANGUAGE MultiWayIf, NamedFieldPuns, RecordWildCards, ScopedTypeVariables #-}

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Bifunctor (first)
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Ord
import Data.Time (UTCTime(..), getCurrentTime)
import Debug.Trace
import Safe
import System.Console.ANSI
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Process.Typed
import Terminal.Game
import qualified Text.Show.Pretty as PrettyShow (ppShow, pPrint)
import qualified Text.Pretty.Simple as PrettySimple (pShow, pShowNoColor, pPrint, pPrintNoColor)
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

usage termsize msoxpath sstate@SavedState{..} = (banner++) $ init $ unlines [
   ""
  ,"caverunner "++version++" - a small terminal arcade game by Simon Michael."
  ,"--------------------------------------------------------------------------------" -- 80
  ,"Thrillseeking drone pilots dive the solar system's caves, competing for glory!"
  ,"Each cave and speed has a high score. Reaching the bottom unlocks more caves."
  ,"How fast, how deep, how far can you go ?"
  ,""
  ,"Usage:"
  ,"caverunner.hs                            # install deps, compile, run the game"
  ,"caverunner [SPEED [CAVE]]                # run the game"
  ,"caverunner --scores|-s                   # show high scores"
  ,"caverunner --print-cave [CAVE [DEPTH]]   # show the cave on stdout"
  ,"caverunner --help|-h                     # show this help"
  ,""
  ,"SPEED sets a different maximum speed (difficulty), from 1 to "++show maxmaxspeed++" (default "++show defmaxspeed++")."
  ,"CAVE selects a different cave, 1 to <highest completed at SPEED + "++show cavelookahead++"> (max "++show maxcavenum++")."
  ,""
  ,"Your terminal size is "++termsize++". (80x25 terminals are best for competition play.)"
  ,soundMessage msoxpath
  ,progressMessage sstate
  ]

soundMessage Nothing = init $ unlines [
   "To enable sound effects, install sox in PATH:"
  ,"apt install sox, brew install sox, choco install sox.portable, or similar."
  ]
soundMessage (Just soxpath) =
  "Sound is enabled, using " ++ soxpath ++ ". --no-sound to disable."
      
progressMessage sstate@SavedState{..} = unlines [
  --  unlockedCavesMessage sstate
   "Currently playing at speed "++show currentspeed
   ++" (cave "++show currentcave++")"
   ++ "; your best score is " ++ show highscore ++ "."
  ]
  where
    highscore = fromMaybe 0 $ M.lookup (currentspeed,currentcave) highscores

unlockedCavesMessage SavedState{..} =
  "At speed " ++ show currentspeed ++ " you have completed " ++ cavecompleted ++ "."  -- ++ ", and can reach " ++ caves ++ "."
  where
    mhighcave = M.lookup currentspeed highcaves
    cavecompleted = case mhighcave of
      Nothing -> "no caves"
      Just c  -> "cave "++show c
    maxcave = fromMaybe 0 mhighcave + cavelookahead
    caves = if maxcave == 1 then "cave 1" else "caves 1 to " ++ show maxcave

-- Print high scores to stdout.
printScores = do
  sstate@SavedState{..} <- getSavedState
  clearScreen >> setCursorPosition 0 0
  putStrLn $ banner
  putStrLn "HIGH SCORES"
  putStrLn "-----------"
  let 
    highscoresl = reverse $ M.toList highscores
    speeds = reverse $ nub $ sort $ map (fst.fst) highscoresl
    scores = nub $ sort $ map snd highscoresl
    scorew = length $ show (maximumDef 0 scores)
    highscoresbyspeed = [
      (speed, [(ca,sc) | ((sp,ca),sc) <- highscoresl, sp==speed])
      | speed <- speeds
      ]

  putStr "   cave:"
  forM_ [1..maxcavenum] $ \ca -> printf (" %"++show scorew++"d") ca
  putStr "\n"
  putStrLn "speed:"
  forM_ (reverse [5,10..maxmaxspeed]) $ \sp -> do
    let mscs = lookup sp highscoresbyspeed
    when (sp >= defmaxspeed || isJust mscs) $ do
      printf "%s %2d    " (if sp == currentspeed then ">" else " ") sp
      forM_ [1..maxcavenum] $ \ca -> do
        let msc = mscs >>= lookup ca
        putStr $ maybe (replicate scorew ' '++"-") (printf (" %"++show scorew++"d")) msc
      putStr "\n"
  putStr "\n"
  putStr $ progressMessage sstate

-------------------------------------------------------------------------------
-- tweakable parameters

(leftkey,rightkey) = (',','.')
wallchar           = '#'
spacechar          = ' '
crashchar          = '*'
fps                = 60  -- target frame rate; computer/terminal may not achieve it
restartdelaysecs   = 3   -- minimum pause after game over
minhelpsecs        = 5   -- minimum time to show onscreen help in first game
completionbonusdelaysecs   = 2
completionadvancedelaysecs = 4

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
maxcavenum  = 10  -- limit to 10 v1 caves
defmaxspeed = 15
maxmaxspeed = 60
cavelookahead = 1

cavespeedinit  = 1      -- nominal initial cave scroll speed (= player's speed within the cave)
cavespeedaccel = 1.008  -- multiply speed by this much each game tick (gravity)
cavespeedbrake = 1      -- multiply speed by this much each player movement (autobraking)

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
-- main types

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

-- A game scene/mode. Some scenes may progress through numbered phases which start at 1.
data Scene =
    Playing
  | Crashed
  | Complete Int  -- 1 show game over, 2 show score bonus / high score, 3 show press a key
  deriving (Show,Eq)

-- In-memory state for a single cave run. A player might consider
-- several of these one "game" if they complete multiple runs without crashing.
data GameState = GameState {
  -- read-only app state
   stats           :: Bool       -- whether to show statistics
  -- current app state
  ,firstgame       :: Bool       -- is this the first game since app start ? (affects help)
  ,controlspressed :: Bool       -- have any movement keys been pressed since game start ? (affects help)
  ,showhelp        :: Bool       -- keep showing the on-screen help ?
  ,pause           :: Bool       -- keep things paused ?
  ,restarttimer    :: Timed Bool -- delay after game over before allowing a restart keypress
  ,restart         :: Bool       -- is it time to restart after game over ?
  ,exit            :: Bool       -- is it time to exit the app ?
  ,scene           :: Scene      -- current app/game mode
  -- current game state
  ,gamew           :: Width      -- width of the game  (but perhaps not the screen)
  ,gameh           :: Height     -- height of the game (and usually the screen)
  ,gtick           :: Integer    -- ticks elapsed within the current game
  ,stick           :: Integer    -- ticks elapsed within the current scene
  ,cavenum         :: CaveNum
  ,highscore       :: Score      -- high score for the current cave and max speed
  ,highscorecopy   :: Score      -- a copy to help detect new high score during cave end scene
  ,score           :: Score      -- current score in this game
  ,scorebonus      :: Score      -- during cave complete scene: bonus awarded for this cave
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

newGameState firstgame stats w h cavenum maxspeed hs = GameState {
   stats           = stats
  ,firstgame       = firstgame
  ,controlspressed = False
  ,showhelp        = True
  ,pause           = False
  ,restart         = False
  ,exit            = False
  ,restarttimer    = creaBoolTimer $ secsToTicks restartdelaysecs
  ,scene           = Playing
  ,gamew           = w
  ,gameh           = h
  ,gtick           = 0
  ,stick           = 0
  ,cavenum         = cavenum
  ,highscore       = hs
  ,highscorecopy   = hs
  ,score           = 0
  ,scorebonus      = 0
  ,randomgen       = mkStdGen cavenum
  ,cavesteps       = 0
  ,cavelines       = []
  ,cavewidth       = cavewidthinit
  ,cavecenter      = half w
  ,cavespeed       = initspeed
  ,cavespeedmin    = initspeed
  ,cavespeedmax    = fromIntegral maxspeed
  ,cavetimer       = newCaveTimer initspeed
  ,speedpan        = 0
  ,playery         = playerYMin h
  ,playerx         = half w
  ,playerchar      = 'V'
  }
  where
    initspeed = fromIntegral maxspeed / 6

-------------------------------------------------------------------------------
-- persistence
-- We save persistent state by logging events to a log file, from which
-- the app's SavedState (game progress, high scores, crash sites..) is calculated.
-- This is robust, forgiving, simple, and efficient enough for now.
-- Older save formats are also supported.

-- Where we save persistent data.
getSaveDir :: IO FilePath
getSaveDir = getXdgDirectory XdgData progname

-- Try to read a value from the named save file, removing newlines first
-- (so it can read either standard or prettified show output). 
-- Returns Just value if successful, Nothing if the file does not exist,
-- or throws an IO error if reading fails.
load :: (Read a, Show a, Eq a) => FilePath -> IO (Maybe a)
load filename = do
  d <- getSaveDir
  let f = d </> filename
  exists <- doesFileExist f
  if not exists
  then pure Nothing
  else do
    s <- filter (/='\n') <$> readFileStrictly f
    case readMay s of
      Nothing -> throwIO $ userError $ init $ unlines [  -- XXX don't print "user error"
           "could not read " ++ f
          ,"Perhaps the format has changed ? Please move it out of the way and run again."
          ]
      Just v -> return $ Just v

-- Format 1 & 2, state and scores files.
-- These will be converted to a log file if one does not exist.

-- Convert 1.0alpha's state file to one LoggedEvent describing the most 
-- recently played speed & cave.
-- The state file's last modification time is used as the timestamp.
-- The position & score will be 0.
-- Returns no event if the file does not exist.
stateFileToEvents :: IO [LoggedEvent]
stateFileToEvents = do
  mls <- load statefilename
  case mls of
    Nothing -> return []
    Just (SavedState{currentcave, currentspeed}) -> do
      t <- getModificationTime . (</> statefilename) =<< getSaveDir
      return [Crash t currentcave currentspeed 0 0 0]
  where
    statefilename = "state"
  
-- Convert 1.0alpha's scores file (format 1 or 2) to LoggedEvents.
-- The scores file's last modification time is used as their timestamps.
-- The columns will be 0. The score will be equal to the depth reached
-- (old scoring system). Returns no events if the file does not exist.
scoresFileToEvents :: IO [LoggedEvent]
scoresFileToEvents = do
  -- load an old scores file if possible, trying format 2 then 1,
  mls <- load scoresfilename <|> ((fromHighScores1 <$>) <$> load scoresfilename)
  case mls of
    Nothing -> return []
    Just ss -> do
      t <- getModificationTime . (</> scoresfilename) =<< getSaveDir
      let
        scoreToEvent HighScore{..} =
          -- in the scores files, score = depth reached and all caves are 462 deep
          (if hscore==462 then Compl else Crash) t hspeed hcave hscore 0 hscore
      return $ map scoreToEvent ss
  where
    scoresfilename = "scores"
    fromHighScores1 :: HighScores1 -> HighScores
    fromHighScores1 scores1 = sort [newHighScore{hcave=c,hspeed=s,hscore=sc} | ((c,s),sc) <- M.toList scores1]

-- Format 1 high scores.
type HighScores1 = M.Map (CaveNum, MaxSpeed) Score

-- Format 2 high scores.
-- These invariants can be expected:
-- - it is always sorted upward (lowest caves, speeds, scores first)
-- - there is at most one score for each cave & speed.
type HighScores = [HighScore]

-- A single format 2 high score for a cave and speed.
data HighScore = HighScore {
   hcave   :: CaveNum
  ,hspeed  :: MaxSpeed
  ,hscore  :: Score
} deriving (Show,Read,Eq,Ord)

newHighScore = HighScore {
   hcave   = 0
  ,hspeed  = 0
  ,hscore  = 0
  }

-- Format 3 log file.

data LoggedEvent3 =
    Ended UTCTime CaveNum MaxSpeed CaveRow GameCol Score YN  -- ^ End of a cave run, with cave, speed, final position, score and whether completed.
  deriving (Show,Read,Eq,Ord)

data YN = Y | N deriving (Show,Read,Eq,Ord)

fromFormat3 :: LoggedEvent3 -> LoggedEvent
fromFormat3 ev@(Ended t ca sp r c sc compl) 
  | ca > maxcavenum = Other $ show ev   -- ignore scores for no-longer-playable caves
  | otherwise = (if compl==Y then Compl else Crash) t sp ca r c sc

-- Format 4 (current).

-- Types of event we can log.
-- These are stored in chronological order in an event log file.
-- Events converted from old save files may have some missing (0) or approximated fields.
data LoggedEvent =
    Other String                                          -- ^ A line found in the log which we couldn't parse.
  | Crash UTCTime MaxSpeed CaveNum CaveRow GameCol Score  -- ^ End of an incomplete cave run, with speed, cave, final position and score.
  | Compl UTCTime MaxSpeed CaveNum CaveRow GameCol Score  -- ^ End of a complete cave run, with speed, cave, final position and score.
  deriving (Show,Read,Eq,Ord)

-- Format 5 (next ?)

-- Crash 2021-11-13 01:00:35.630314 UTC 15 1 0 40 0
-- Crash 2021-11-13 01:01:48.73835 UTC 15 1 1 39 1
-- Compl 2021-11-13 01:02:15.612641 UTC 15 1 3 41 153
-- -> 
-- CR1 2021-11-13 01:00:35 UTC 15  1  Crash 000 040     0
-- CR1 2021-11-13 01:01:48 UTC 15  1  Crash 001 039     1
-- CR1 2021-11-13 01:02:15 UTC 15  1  Compl 003 041   153

-- Get the file path of the event log, which contains chronologically ordered events,
-- one per line, from which we can calculate state and statistics.
logPath :: IO FilePath
logPath = getSaveDir <&> (</> logfilename) where logfilename = "log"

-- Append some events to the event log on disk, creating it and its directory if necessary.
logAppend :: [LoggedEvent] -> IO ()
logAppend evs = do
  f <- logPath
  let d = takeDirectory f
  createDirectoryIfMissing True d
  appendFile f $ unlines $ map pshow evs

-- If the event log file doesn't exist, but the old state and scores
-- files do, generate it from those. Called at app start.
logMigrate :: IO ()
logMigrate = do
  exists <- logPath >>= doesFileExist
  unless exists $ do
    scoresevs <- scoresFileToEvents
    stateevs  <- stateFileToEvents
    logAppend $ scoresevs ++ stateevs

-- Parse one line of the event log if possible, trying old and new formats.
readLogLine :: String -> LoggedEvent
readLogLine s = fromMaybe (Other s) $ fromFormat3 <$> readMay s <|> readMay s
  
-- Read all events from the event log file, in chronological order.
-- Any unreadable lines are parsed as Other events.
-- If the file doesn't exist, returns an empty list. Other problems will raise an IO exception.
logRead :: IO [LoggedEvent]
logRead = do
  f <- logPath
  exists <- doesFileExist f
  if exists
  then readFile f <&> map readLogLine . lines
  else return []

-- Long-term persistent app state, as represented in memory.
data SavedState = SavedState {
   currentspeed :: MaxSpeed                         -- the current speed being played
  ,currentcave  :: CaveNum                          -- the current cave being played at this speed
  ,highcaves    :: M.Map MaxSpeed CaveNum           -- the highest cave completed at each speed
  ,highscores   :: M.Map (MaxSpeed, CaveNum) Score  -- the highest score achieved in each cave at each speed
} deriving (Read, Show, Eq)

newSavedState = SavedState{
   currentspeed = defmaxspeed
  ,currentcave  = defcavenum
  ,highcaves    = M.empty
  ,highscores   = M.empty
}

-- Calculate app saved state from events.
eventsToSavedState :: [LoggedEvent] -> SavedState
eventsToSavedState evs = newSavedState{
    currentspeed = lastspeed
  , currentcave  = lastcave
  , highcaves    = M.fromList highcaves
  , highscores   = M.fromList highscores
  }
  where
    (lastspeed, lastcave) = case lastMay evs of
           Just (Crash _ sp ca _ _ _) -> (sp, ca)
           Just (Compl _ sp ca _ _ _) -> (sp, ca)
           _ -> (defmaxspeed, defcavenum)
    highcaves = 
      catMaybes $
      map (maximumByMay (comparing snd)) $
      groupBy (\a b -> fst a == fst b) $ 
      nub $ sort $
      [(sp, ca) | Compl _ sp ca _ _ _ <- evs]
    highscores =
      catMaybes $
      map (maximumByMay (comparing snd)) $
      groupBy (\a b -> fst a == fst b) $ 
      nub $ sort $ 
      catMaybes $ map eventScore evs
      where
        eventScore (Crash _ sp ca _ _ sc) = Just ((sp,ca), sc)
        eventScore (Compl _ sp ca _ _ sc) = Just ((sp,ca), sc)
        eventScore _ = Nothing

-- Read the app's persistent state from the log file.
getSavedState :: IO SavedState
getSavedState = logRead <&> eventsToSavedState

-- Update the in-memory copy of persistent state with the last run's 
-- speed, cave number, score, and the cave number to be played next.
savedStateUpdate speed lastcave score nextcave sstate@SavedState{..} =
  sstate{
     currentspeed = speed
    ,currentcave  = nextcave
    ,highscores   = M.insertWith max (speed, lastcave) score highscores
    ,highcaves    = M.insertWith max speed nextcave highcaves
    }

-------------------------------------------------------------------------------
-- app logic

main = do
  logMigrate
  sstate@SavedState{..} <- getSavedState
  args <- getArgs
  when ("-h" `elem` args || "--help" `elem` args) $ exitWithUsage sstate
  let
    (flags, args') = partition ("-" `isPrefixOf`) args
    (speed, cavenum, hascavearg) =
      case args' of
        []    -> (currentspeed, currentcave, False)
        [s]   -> (checkspeed $ readDef (speederr s) s, currentcave, False)
        [s,c] -> (sp, checkcave sp $ readDef (caveerr c) c, True)
          where sp = checkspeed $ readDef (speederr s) s
        _     -> err "too many arguments, please see --help"
        where
          checkspeed s = if s >= 1 && s <= maxmaxspeed then s else speederr $ show s
          speederr a = err $ "SPEED should be 1-"++show maxmaxspeed++" (received "++a++"), see --help)"
          checkcave s c
            | c <= highcave + cavelookahead = c
            | otherwise = err $ init $ unlines [
                 ""
                ,"To reach cave "++show c ++ " at speed "++show s ++ ", you must complete at least cave "++ show (c-cavelookahead) ++ "."
                ,unlockedCavesMessage sstate
                ]
            where highcave = fromMaybe 0 $ M.lookup s highcaves
          caveerr a = err $ "CAVE should be 1-"++show maxcavenum++" (received "++a++"), see --help)"

  cavenum `seq` if
    --  | "--print-speed-sound-volume" `elem` flags -> printSpeedSoundVolumes

    | "-s" `elem` args || "--scores" `elem` flags -> printScores

    | "--print-cave" `elem` flags ->
      let mdepth = if hascavearg then Just cavenum else Nothing
      in printCave cavenum mdepth

    | otherwise -> do
      let sstate' = sstate{ currentcave=cavenum, currentspeed=speed }
      playGames True ("--stats" `elem` flags) (fromIntegral speed) cavenum sstate'

-- Generate the cave just like the game would, printing each line to stdout.
-- Optionally, limit to just the first N lines.
printCave cavenum mdepth = do
  putStrLn $ progname ++ " cave "++show cavenum
  go mdepth $ newGameState False False gamewidth 25 cavenum 15 0
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
        putStrLn $ showCaveLineNumbered gamewidth l cavesteps'
        go (fmap (subtract 1) mremaining)
           g{randomgen    = randomgen'
            ,cavesteps    = cavesteps'
            ,cavelines    = cavelines'
            ,cavewidth    = cavewidth'
            ,cavecenter   = cavecenter'
            }

-- Play the game repeatedly at the given cave and speed,
-- updating save files and/or advancing to next cave when appropriate.
-- The first arguments specify if this is the first game of a session and
-- if onscreen dev stats should be displayed.
playGames :: Bool -> Bool -> MaxSpeed -> CaveNum -> SavedState -> IO ()
playGames firstgame showstats maxspeed cavenum sstate@SavedState{..} = do
  (screenw,screenh) <- displaySize  -- use full screen height for each game (apparently last line is unusable on windows ? surely it's fine)
  let
    highscore = fromMaybe 0 $ M.lookup (maxspeed, cavenum) highscores
    game = newGame firstgame showstats screenh cavenum maxspeed highscore

  -- run one game. Will exit if terminal is too small.
  g@GameState{score,exit,playerx} <- Terminal.Game.playGameS game

  -- game ended by crashing, reaching cave end, or quitting with q. (Ctrl-c is not caught here.)
  -- persistent state: log an end event
  let atend = playerAtEnd g
  let evcons = if atend then Compl else Crash
  t <- getCurrentTime
  logAppend [evcons t maxspeed cavenum (playerDepth g) playerx score]
  -- in-memory state: update high caves/scores, and
  -- if the end was reached, advance to next cave or speed
  let
    (cavenum', maxspeed')
      | not atend             = (cavenum,   maxspeed)
      | cavenum < maxcavenum  = (cavenum+1, maxspeed)
      | otherwise             = (1,         maxspeed+5)
    sstate' = savedStateUpdate maxspeed cavenum score cavenum' sstate

  -- play again, or exit the app
  if not exit
  then 
    playGames False showstats maxspeed cavenum' sstate'
  else do
    putStr $ progressMessage sstate'
    putStrLn ""
    printScores
    when soundEnabled quitSound

-- Initialise a new game (a cave run).
newGame :: Bool -> Bool -> Height -> CaveNum -> MaxSpeed -> Score -> Game GameState
newGame firstgame stats gameh cavenum maxspeed hs =
  Game { gScreenWidth   = gamewidth, -- width used (and required) for drawing (a constant 80 for repeatable caves)
         gScreenHeight  = gameh,     -- height used for drawing (the screen height)
         gFPS           = fps,       -- target frames/game ticks per second
         gInitState     = newGameState firstgame stats gamewidth gameh cavenum maxspeed hs,
         gLogicFunction = step',
         gDrawFunction  = draw,
         gQuitFunction  = timeToQuit
       }

-------------------------------------------------------------------------------
-- event handlers & game logic for each scene

-- Before calling step, do updates that should happen on every tick no matter what.
step' g@GameState{..} Tick = step g' Tick
  where
    g' = g{
       gtick=gtick+1
      ,stick=stick+1
      ,showhelp=showhelp && not (timeToHideHelp g)
      }
step' g ev = step g ev

step g@GameState{scene=Playing, ..} (KeyPress k)
  | k == 'q'              = g { exit=True }
  | k `elem` "p ", pause  = g { pause=False }
  | k `elem` "p "         = g { pause=True,  showhelp=True }
  | not pause, k == leftkey =
      g { playerx   = max 1 (playerx - 1)
        , cavespeed = max cavespeedmin (cavespeed * cavespeedbrake)
        , controlspressed = True
        }
  | not pause, k == rightkey =
      g { playerx   = min gamew (playerx + 1)
        , cavespeed = max cavespeedmin (cavespeed * cavespeedbrake)
        , controlspressed = True
        }
  | otherwise = g

step g@GameState{scene=Playing, ..} Tick =
  let
    starting = gtick==1
    gameover = playerCrashed g
    victory  = playerAtEnd g
  in
    if
      | pause -> g  -- paused

      | starting -> unsafePlay gameStartSound g

      | gameover ->  -- crashed or reached the end
        unsafePlay (if victory then victorySound else crashSound cavespeed) $
        g{ scene        = if victory then Complete 1 else Crashed
          ,stick        = 0
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
           cavelines') = stepCave     g
          speedpan'    = stepSpeedpan g cavesteps' cavespeed' cavelines'
          score'       = stepScore    g cavesteps'
          walldist     = fromMaybe 9999 $ playerWallDistance g
        in
          -- (if cavesteps `mod` 5 == 4 -- && cavespeed' > 5 
          --   then unsafePlay $ speedSound cavespeed' else id) $
          -- XXX trying very hard to start the beeps at cave mouth, no success
          -- (if depth>0 && (depth-1) `mod` 5 == 1 
          -- (if cavesteps'>playerHeight g && cavesteps' `mod` 5 == 0
          (if cavesteps `mod` 5 == 2
            then unsafePlay $ depthSound cavesteps' else id) $
          (if walldist <= 2 then unsafePlay $ closeShaveSound walldist else id) $
          -- (if score <= highscore && score' > highscore then unsafePlay inGameHighScoreSound else id) $
          g{ randomgen    = randomgen'
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
        let (cavespeed', cavespeedmin') = stepSpeed g
        in
          g{ cavetimer    = tick cavetimer
            ,cavespeed    = cavespeed'
            ,cavespeedmin = cavespeedmin'
            }

-- completionbonusdelaysecs into the cave complete scene, add score bonus and maybe play/show high score message
step g@GameState{scene=Complete 1, ..} Tick
  | stick > secsToTicks completionbonusdelaysecs =
      (if score > highscore then unsafePlay endGameHighScoreSound else id) $
      g{scene=Complete 2
       ,restarttimer = tick restarttimer
       ,score      = score'
       ,scorebonus = bonus  -- save this for display
       ,highscore  = max highscore score'
       }
  where
    score' = score + bonus
    bonus = round cavespeedmax * completionbonusmultiplier
      where completionbonusmultiplier = 10

step g@GameState{scene=Complete _, ..} Tick = g{restarttimer = tick restarttimer}

step g@GameState{scene=Crashed, ..} Tick = g{restarttimer = tick restarttimer}

step g@GameState{..} (KeyPress 'q') = g { exit = True }

-- XXX trying to support pause during game over
-- step g@GameState{..} (KeyPress k)
--   | k `elem` " p"         = g{pause=True}
--   | k `elem` " p", pause  = g{pause=False}
--   | gameOver g, isExpired restarttimer = g{restart=True}
--   | gameOver g = g

step g@GameState{..} (KeyPress _) | gameOver g, isExpired restarttimer = g{restart=True}

step g@GameState{..} (KeyPress _) | gameOver g = g

step g@GameState{..} (KeyPress k)
  | k `elem` " p"         = g{pause=True}
  | k `elem` " p", pause  = g{pause=False}
  | otherwise             = g

-- step GameState{..} Tick = error $ "No handler for " ++ show scene ++ " -> " ++ show Tick ++ " !"

-------------------------------------------------------------------------------
-- logic helpers

-- Should the current game be ended ?
timeToQuit g@GameState{..}
  | exit = True           -- yes if q was pressed
  | pause = False         -- no if paused
  | restart = True        -- yes if a key was pressed after game over
  | otherwise = False

-- If onscreen help is being shown, is it time to hide it yet ?
-- In the first game of a session, true after a certain amount of game time
-- and any movement keys have been pressed and the game is not paused.
-- In subsequent games, true when the game is not paused.
timeToHideHelp GameState{..}
  | firstgame = gtick > secsToTicks minhelpsecs && controlspressed && not pause
  | otherwise = not pause

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

-- speedpan - as speed increases, maybe pan the viewport up (player and walls move down)
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

gameOver GameState{scene} = case scene of
  Crashed    -> True
  Complete _ -> True
  _          -> False

-- Create a timer for the next cave step, given the desired steps/s.
-- The steps/s should be no greater than ticks/s (the frame rate),
-- and will be capped at that (creating a one-tick timer).
newCaveTimer stepspersec = creaBoolTimer $ max 1 (secsToTicks  $ 1 / stepspersec)

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
-- utilities

readFileStrictly f = readFile f >>= \s -> evaluate (length s) >> return s

-- pretty-show: can't print in colour and had some other drawback
-- (couldn't print times, maybe fixed now ?) But it generates more
-- compact and human-readable output than pretty-simple.

pshow, ps :: Show a => a -> String
pshow = PrettyShow.ppShow

ps = pshow

pprint, pp :: Show a => a -> IO ()
pprint = PrettyShow.pPrint

pp = pprint

-- pshow :: Show a => a -> String
-- pshow = T.unpack . PrettySimple.pShowNoColor

-- pprint :: Show a => a -> IO ()
-- pprint = PrettySimple.pPrintNoColor

-- pshowc :: Show a => a -> String
-- pshowc = T.unpack . PrettySimple.pShow

pprintc :: Show a => a -> IO ()
pprintc = PrettySimple.pPrint

exitWithUsage sstate = do
  clearScreen >> setCursorPosition 0 0
  termsize <- displaySizeStrSafe
  msox <- findExecutable "sox"
  putStr $ usage termsize msox sstate
  exitSuccess

displaySizeStrSafe = handle (\(_::ATGException) -> return "unknown") $ do
  (w,h) <- displaySize
  return $ show w++"x"++show h

-- Convert seconds to game ticks based on global frame rate.
secsToTicks :: Float -> Integer
secsToTicks = round . (* fromIntegral fps)

half :: Integral a => a -> a
half = (`div` 2)

err = errorWithoutStackTrace

-- Run a shell command, either synchronously or in a background process,
-- ignoring any output, errors or exceptions. ("Silent" here does not mean sound.)
runProcessSilent :: Bool -> String -> IO ()
runProcessSilent synchronous =
  silenceExceptions .
  (if synchronous then void else void . forkIO) . void . runProcess .
  silenceOutput .
  shell

silenceExceptions :: IO () -> IO ()
silenceExceptions = handle (\(e::IOException) -> -- trace (show e) $ 
  return ())

silenceOutput :: ProcessConfig i o e -> ProcessConfig i () ()
silenceOutput = setStdout nullStream . setStderr nullStream

-------------------------------------------------------------------------------
-- drawing for each scene

draw g@GameState{..} =
    blankPlane gamew gameh
  & (max 1 (gameh - length cavelines + 1), 1) % drawCave g
  -- & (1, 1)          % blankPlane gamew 1
  & (1, titlex)     % drawTitle g
  & (1, cavenamex)  % drawCaveName g
  & (if showhelp then (3, helpx) % drawHelp g else id)
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
    gameoverw  = 48
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

showCaveLineNumbered gamew l n =
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
      (stringPlane (" "++[leftkey]++" ")  #bold  ||| stringPlane " left ")
  === (stringPlane (" "++[rightkey]++" ") #bold  ||| stringPlane " right ")
  === (stringPlane "spc" #bold  ||| if pause then stringPlane " pause " #bold else stringPlane " pause ")
  === (stringPlane " q " #bold  ||| if exit then stringPlane " quit " #bold else stringPlane " quit ")

drawHighScore g@GameState{..} =
  stringPlane " high score " ||| (stringPlane (printf "%04d " highscore) & maybebold)
  where
    maybebold = if gameOver g && highscore > highscorecopy then (#bold) else id

drawScore GameState{..} =
  stringPlane " score " ||| (stringPlane (printf "%04d " score) & maybebold)
  where
    maybebold = if score > highscorecopy then (#bold) else id

drawSpeed g@GameState{..} = stringPlane " speed " ||| stringPlane (printf "%3.f " cavespeed)

drawStats g@GameState{..} =
      (stringPlane "    depth " ||| stringPlane (printf "%3d " (playerDepth g)))
  -- === (stringPlane "depthmod5 " ||| stringPlane (printf "%3d " (playerDepth g `mod` 5)))
  === (stringPlane "    width " ||| stringPlane (printf "%3d " cavewidth))
  === (stringPlane " minspeed " ||| stringPlane (printf "%3.f " cavespeedmin))
  -- === (stringPlane " speedpan " ||| stringPlane (printf "%3d " speedpan))
  -- === (stringPlane "    speed " ||| stringPlane (printf "%3.f " cavespeed))
  === (stringPlane "   gtick " ||| stringPlane (printf "%4d " gtick))
  === (stringPlane "   stick " ||| stringPlane (printf "%4d " stick))
  === (stringPlane "scene " ||| stringPlane (printf "%-7s " (showSceneCompact 7 scene)))
  -- === (stringPlane "   bonus " ||| stringPlane (printf "%4d " scorebonus))
  === (stringPlane "   score " ||| stringPlane (printf "%4d " score))
  === (stringPlane "  hscore " ||| stringPlane (printf "%4d " highscore))
  === (stringPlane "hscorecp " ||| stringPlane (printf "%4d " highscorecopy))

showSceneCompact width scene = 
  case scene of
    Complete n -> take (width-length sn) ss ++ sn
      where sn = show n
    _ -> take width ss
  where ss = show scene
 
drawGameOver g@GameState{..} w h =
  box w h '-'
  & (2,2) % box (w-2) (h-2) ' '
  & (y1,x1) % stringPlane l1
  & (y2,x2) % stringPlane l2
  & (if isExpired restarttimer then (y3,x3) % stringPlane l3 else id)
  where
    showline2 = stick > secsToTicks completionbonusdelaysecs
    showline3 = stick > secsToTicks completionadvancedelaysecs
    -- printf makes runtime errors, be careful
    [l1,l2,l3] = case scene of
      Crashed -> [
         "GAME OVER"
        ,if showline2 then printf "You reached depth %d.%s" (playerDepth g) hs else ""
        ,if showline3 then "Press a key to relaunch.." else ""
        ]
      Complete n -> [
         printf "CAVE %d COMPLETE" cavenum
        ,if showline2 then printf "Nice flying! Bonus: %d.%s" scorebonus hs else ""
        ,if showline3 then "Press a key to advance.." else ""
        ]
    hs = if score > highscorecopy then " New high score!" else ""
    xstart str = 2 + half innerw - half (fromIntegral $ length str) where innerw = w - 2
    (y1, x1) = (3, xstart l1)
    (y2, x2) = (4, xstart l2)
    (y3, x3) = (5, xstart l3)

-------------------------------------------------------------------------------
-- sound helpers

-- Check whether sounds should be played - true unless the program was
-- run with the --no-sound flag. Uses unsafePerformIO. Won't change in a GHCI session.
soundEnabled :: Bool
soundEnabled = "--no-sound" `notElem` unsafePerformIO getArgs

-- Execute an IO action (typically one that plays a sound asynchronously)
-- before evaluating the second argument, unless sound is disabled.
-- Uses unsafePerformIO.
unsafePlay :: IO a -> b -> b
unsafePlay a = if soundEnabled then seq (unsafePerformIO a) else id

-- Try to play a synthesised sound by running `sox synth` with the given arguments, 
-- optionally blocking until the sound finishes playing, otherwise forking a background process to play it.
-- If no SynthArgs are provided, this plays a one-second tone.
-- This should never fail, and ignores all errors (so uncomment the trace if you need to troubleshoot).
--
-- Tips:
-- When possible, combine multiple notes into a single sox sound and call this once, 
-- minimising subprocesses and audible gaps between notes.
-- sox seems to truncate sounds at the end for some reason; you might need to add
-- some extra duration or a final delay to hear a sound in full.
--
soxPlay :: Bool -> SynthArgs -> IO ()
soxPlay synchronous args = runProcessSilent synchronous $
  -- traceShowId $
  "sox -V0 -qnd synth " ++ unwords (if null args then ["1"] else args)

-- Arguments to follow sox's `synth` command, such as ["1","sin","200"] (a 1 second 200hz sine wave).
-- Should be one or more `synth` arguments, optionally followed by any other sox arguments,
-- which allows complex sounds, chords and note sequences to be generated.
type SynthArgs = [String]

-------------------------------------------------------------------------------
-- sound effects. These should generally be asynchronous, returning immediately.

gameStartSound = void $ forkIO $ do
  let
    short = 0.12
    long  = 0.5
    v     = 0.7
  soxPlay False [
               show short,"sin 400-100 vol",show v
    ,": synth",show short,"sin 400-100 vol",show v
    ,": synth",show long ,"sin 400-100 vol",show v
    ]

depthSound depth = do
  let v = 0.3
  soxPlay False [".15", 
    "sin", show $ 100 + depth,
    "sin", show $ 1000 - depth, 
    "vol", show v
    ]

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
    v | distance<=1 = 0.3
      | distance<=2 = 0.1
      | otherwise   = 0
  soxPlay False [
    show d, "brownnoise",
    "fade p .2 -0 0",
    "vol", show v
    ]

crashSound speed = do
  soxPlay False [show d, "brownnoise", "fade", "l", "0", "-0", show d, "vol", show v]
  where
    speed' = min (fromIntegral maxmaxspeed) $ 30 + speed / 2
    d = speed' / 6
    v = crashSoundVolume speed'

crashSoundVolume s = s / 10

printCrashSoundVolumes = do
  putStrLn "        volume"
  putStrLn "speed       123456789"
  putStr $ unlines $ reverse $ [printf "%5.f  %4.1f " s v ++ replicate (round $ v * 10) '*' | (s,v) <- vols]
  where vols = [(s, crashSoundVolume s) | s <- [0,5..fromIntegral maxmaxspeed::Speed]]

inGameHighScoreSound = soxPlay False [".1 sin 800 sin 800 delay 0 +.2"]

endGameHighScoreSound =
  soxPlay False [
    -- ".05 sin 400 sin 500 sin 600 sin 800"
    ".05 sin 400 sin 500 sin 600 sin 800 sin 800"  -- extra end note in case of truncation
    ,"delay", overalldelay  -- not used any more but tricky to remove
    -- ,"+.1 +.1 +.1"
    ,"+.1 +.1 +.1 +.2"
    ]
  where
    overalldelay = "0" -- show $ restartdelaysecs / 2

victorySound = do
  soxPlay False [
             "0.06 sin 200 sin 300 sin 400 remix -"
    ,": synth 2 sin 200 sin 300 sin 400 remix - delay .06 fade h 0 -0 .5"
    ]

dropSound = soxPlay False [".8","sin","300-1"]

-- synchronous, so it can play before app exits
quitSound = soxPlay True [".3","sin","200-100"]
