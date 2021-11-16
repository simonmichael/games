#!/usr/bin/env stack
{- stack script --optimize --verbosity=warn --resolver=nightly-2021-11-15
  --ghc-options=-threaded
  --package ansi-terminal
  --package ansi-terminal-game
  --package containers
  --package directory
  --package extra
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
import Data.Bifunctor (second)
import Data.Char (chr,ord,isSpace)
import Data.Function (fix)
import Data.Functor ((<&>))
import Data.List
import Data.List.Extra
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

printBanner = do
  putStrLnAnsi [bold_, fgvividblue]   "  _________ __   _____  _______  ______  ____  ___  _____"
  putStrLnAnsi [bold_, fgvividgreen]  " / ___/ __ `/ | / / _ \\/ ___/ / / / __ \\/ __ \\/ _ \\/ ___/"
  putStrLnAnsi [bold_, fgvividyellow] "/ /__/ /_/ /| |/ /  __/ /  / /_/ / / / / / / /  __/ /    "
  putStrLnAnsi [bold_, fgvividred]    "\\___/\\__,_/ |___/\\___/_/   \\__,_/_/ /_/_/ /_/\\___/_/     "

usage termsize msoxpath sstate@SavedState{..} = init $ unlines [
   ""
  ,"caverunner "++version++" - a small terminal arcade game by Simon Michael."
  ,"--------------------------------------------------------------------------------" -- 80
  ,"Thrillseeking drone pilots dive the solar system's caves, competing for glory!"
  ,"Each cave and speed has a high score. Reaching the bottom unlocks more caves."
  ,"How fast, how deep, how far can you go ?"
  ,""
  ,"Usage:"
  ,"caverunner.hs                         # install deps, compile, run the game"
  ,"caverunner [SPEED [CAVE]]             # run the game"
  ,"caverunner -s|--scores                # show high scores"
  ,"caverunner -p|--print [CAVE [DEPTH]]  # show the cave on stdout"
  ,"caverunner -h|--help                  # show this help"
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

-------------------------------------------------------------------------------
-- tweakable parameters

(leftkey,rightkey) = (',','.')
wallchar           = '#'
spacechar          = ' '
crashchar          = '*'
tps                = 60  -- target frame rate; computer/terminal may not achieve it
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
completionbonusmultiplier = 10  -- award this times max speed when completing a cave

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

-- One line within a cave, with its depth and left/right wall positions.
data CaveLine = CaveLine CaveRow CaveCol CaveCol deriving (Show)

-- Crash sites within a row, and how many times at each one.
type RowCrashes = M.Map CaveCol Int

-- Make a RowCrashes containing one new crash site.
newRowCrash :: CaveCol -> RowCrashes
newRowCrash col = M.fromList [(col, 1)]

-- Add a new crash site to a RowCrashes.
rowCrashesAdd :: CaveCol -> RowCrashes -> RowCrashes
rowCrashesAdd = insertOrUpdate 1 (+1)

-- Crash sites within a cave.
type CaveCrashes = M.Map CaveRow RowCrashes

-- Make a CaveCrashes containing one new crash site.
newCaveCrash :: CaveRow -> CaveCol -> CaveCrashes
newCaveCrash row col = M.fromList [(row, newRowCrash col)]

-- Add a new crash site to a CaveCrashes.
caveCrashesAdd :: CaveRow -> CaveCol -> CaveCrashes -> CaveCrashes
caveCrashesAdd row col = insertOrUpdate (newRowCrash col) (rowCrashesAdd col) row

-- How many crashes have there been at the given cave depth and column ?
caveCrashesAt :: CaveRow -> CaveCol -> CaveCrashes -> Int
caveCrashesAt row col cavecrashes = M.lookup row cavecrashes >>= M.lookup col & fromMaybe 0

-- Crash sites within all caves.
type AllCrashes = M.Map CaveNum CaveCrashes

-- Add a new crash site to an AllCrashes.
allCrashesAdd :: CaveNum -> CaveRow -> CaveCol -> AllCrashes -> AllCrashes
allCrashesAdd cave row col = insertOrUpdate (newCaveCrash row col) (caveCrashesAdd row col) cave


-- A game scene/mode. Some scenes may progress through numbered phases starting at 1.
data Scene =
    Playing
  | RunEnd Bool Int  -- was cave completed ?
                     -- and phase: 1 show game over, 2 show bonus/highscore, 3 (not used) show press a key
  deriving (Show,Eq)

scenePhase Playing = 0
scenePhase (RunEnd _ phase) = phase

-- In-memory state for a single cave run. A player might consider
-- several of these one "game" if they complete multiple runs without crashing.
data GameState = GameState {
  -- read-only app state
   showstats       :: Bool       -- whether to show dev statistics
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
  ,stick           :: Integer    -- ticks elapsed within the current scene while not paused
  ,cavenum         :: CaveNum
  ,highscore       :: Score      -- high score for the current cave and max speed
  ,highscorecopy   :: Score      -- a copy to help detect new high score during cave end scene
  ,score           :: Score      -- current score in this game
  ,scorebonus      :: Score      -- during cave complete scene: bonus awarded for this cave
  ,randomgen       :: StdGen
  ,cavesteps       :: Int        -- how many cave lines have been generated since game start (should be Integer but I can't be bothered)
  ,cavelines       :: [CaveLine] -- recent cave lines, for display; newest/bottom-most first
  ,cavecrashes     :: CaveCrashes -- past crashes in this cave, for display
  ,cavewidth       :: Width      -- current cave width (inner space between the walls)
  ,cavecenter      :: GameCol    -- current x coordinate in game area of the cave's horizontal midpoint
  ,cavespeed       :: Speed      -- current speed of player dive/cave scroll in lines/s, must be <= tps
  ,cavespeedmin    :: Speed      -- current minimum speed player can brake to
  ,cavespeedmax    :: Speed      -- maximum speed player can accelerate to, must be <= tps
  ,cavetimer       :: Timed Bool -- delay before next cave scroll
  ,speedpan        :: Height     -- current number of rows to pan the viewport down, based on current speed
  ,playery         :: GameRow    -- player's y coordinate in game area
  ,playerx         :: GameCol    -- player's x coordinate in game area
  ,playerchar      :: Char
  }
  deriving (Show)

newGameState firstgame w h cave maxspeed hs crashes = GameState {
   showstats       = False
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
  ,cavenum         = cave
  ,highscore       = hs
  ,highscorecopy   = hs
  ,score           = 0
  ,scorebonus      = 0
  ,randomgen       = mkStdGen cave
  ,cavesteps       = 0
  ,cavelines       = []
  ,cavecrashes     = crashes
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
-- CR1 2021-11-13 01:00:35 UTC Crash 15  1  000 040    0
-- CR1 2021-11-13 01:01:48 UTC Crash 15  1  001 039    1
-- CR1 2021-11-13 01:02:15 UTC Compl 15  1  003 041  153

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
  ,allcrashes   :: AllCrashes
} deriving (Read, Show, Eq)

newSavedState = SavedState{
   currentspeed = defmaxspeed
  ,currentcave  = defcavenum
  ,highcaves    = M.empty
  ,highscores   = M.empty
  ,allcrashes   = M.empty
}

-- Calculate app saved state from events.
eventsToSavedState :: [LoggedEvent] -> SavedState
eventsToSavedState evs = newSavedState{
    currentspeed = curspeed
  , currentcave  = curcave
  , highcaves    = M.fromList highcaves
  , highscores   = M.fromList highscores
  , allcrashes   = M.fromList allcrashes
  }
  where
    (curspeed, curcave) = case lastMay evs of
           Just (Crash _ sp ca _ _ _) -> (sp, ca)
           Just (Compl _ sp ca _ _ _) -> (sp, ca+1)
           _ -> (defmaxspeed, defcavenum)

    highcaves = 
      mapMaybe (maximumByMay (comparing snd)) $
      groupBy (\a b -> fst a == fst b) $ 
      nubSort $
      [(sp, ca) | Compl _ sp ca _ _ _ <- evs]

    highscores =
      mapMaybe (maximumByMay (comparing snd)) $
      groupBy (\a b -> fst a == fst b) $ 
      nubSort $ 
      mapMaybe eventScore evs
      where
        eventScore (Crash _ sp ca _ _ sc) = Just ((sp,ca), sc)
        eventScore (Compl _ sp ca _ _ sc) = Just ((sp,ca), sc)
        eventScore _ = Nothing

    allcrashes =
      map (
        second (
          M.fromList .
          map (second (M.fromList . count)) .
          groupSort)
        ) $
      groupSort $
      concatMap eventCrash evs
      where
        eventCrash (Crash _ _ ca r c _) = [(ca, (r,c))]
        eventCrash _ = []

-- Read the app's persistent state from the log file.
getSavedState :: IO SavedState
getSavedState = logRead <&> eventsToSavedState

-- Update the in-memory saved state from the last run as needed
-- (maybe a new high score or high cave).
-- The next cave number is also provided to help set the latter. (XXX ?)
savedStateUpdate :: MaxSpeed -> CaveNum -> Score -> CaveNum -> CaveRow -> CaveCol -> SavedState -> SavedState
savedStateUpdate speed cave score nextcave row col sstate@SavedState{..} =
  sstate{
     highscores   = M.insertWith max (speed, cave) score highscores
    ,highcaves    = M.insertWith max speed nextcave highcaves
    ,allcrashes   = allCrashesAdd cave row col allcrashes
    }

-- What's the saved current cave at the given speed ?
savedStateCurrentCaveAt :: MaxSpeed -> SavedState -> CaveNum
savedStateCurrentCaveAt speed sstate@SavedState{..} =
  maybe defcavenum (+1) $ M.lookup speed highcaves

-------------------------------------------------------------------------------
-- app logic

main = do
  logMigrate
  sstate@SavedState{..} <- getSavedState
  args <- getArgs
  let 
    (flags, args') = partition ("-" `isPrefixOf`) args
    highcave (Just sp) = fromMaybe 0 $ M.lookup sp highcaves
    highcave Nothing   = maximumDef 0 $ M.elems highcaves
    caveerr a = err $ "CAVE should be 1-"++show maxcavenum++" (received "++a++"), see --help)"

  if
    | "-h" `elem` flags || "--help" `elem` flags -> printUsage sstate
    | "-s" `elem` flags || "--scores" `elem` flags -> printScores
    | "-p" `elem` flags || "--print-cave" `elem` flags -> do
      let
        (cave, mdepth) = case args' of
          []    -> (currentcave, Nothing)
          [c]   -> (parseCave c, Nothing)
          [c,d] -> (parseCave c, readMay d)
          _     -> err "too many arguments, please see --help"
          where
            parseCave c = checkcave $ readDef (caveerr c) c
              where
                checkcave c
                  | c <= highcave Nothing + cavelookahead = c
                  | otherwise = err $ init $ unlines [
                      ""
                      ,"To view cave "++show c ++ ", you must complete at least cave "++ show (c-cavelookahead) ++ " (at any speed)."
                      ]
      printCave cave mdepth sstate

    --  | "--print-speed-sound-volume" `elem` flags -> printSpeedSoundVolumes

    | otherwise -> do
      let
        (speed, cave) = case args' of
          []    -> (currentspeed, currentcave)
          [s]   -> (sp, savedStateCurrentCaveAt sp sstate) where sp = parseSpeed s
          [s,c] -> (sp, parseCave sp c)                    where sp = parseSpeed s
          _     -> err "too many arguments, please see --help"
          where
            parseSpeed s = checkspeed $ readDef (speederr s) s
              where
                checkspeed s = if s >= 1 && s <= maxmaxspeed then s else speederr $ show s
                speederr a = err $ "SPEED should be 1-"++show maxmaxspeed++" (received "++a++"), see --help)"
            parseCave sp c = checkcave sp $ readDef (caveerr c) c
              where
                checkcave sp c
                  | c <= highcave (Just sp) + cavelookahead = c
                  | otherwise = err $ init $ unlines [
                      ""
                      ,"To reach cave "++show c ++ " at speed "++show sp ++ ", you must complete at least cave "++ show (c-cavelookahead) ++ "."
                      ,unlockedCavesMessage sstate
                      ]
      playGames True sstate{currentcave=cave, currentspeed=speed}

-- Print help.
printUsage sstate = do
  clearScreen
  setCursorPosition 0 0
  termsize <- displaySizeStrSafe
  msox <- findExecutable "sox"
  printBanner
  putStr $ usage termsize msox sstate

-- Print high scores.
printScores = do
  sstate@SavedState{..} <- getSavedState
  clearScreen
  -- setCursorPosition 0 0  -- omit for now, allow entr to scroll output
  printBanner
  -- putStrLnAnsi [bold_]
  putStrLn "\nHIGH SCORES\n-----------"
  let 
    highscoresl = reverse $ M.toList highscores
    speeds = reverse $ nubSort $ map (fst.fst) highscoresl
    scores = nubSort $ map snd highscoresl
    highscoresbyspeed = [
      (speed, [(ca,sc) | ((sp,ca),sc) <- highscoresl, sp==speed])
      | speed <- speeds
      ]
    total = sum scores
    weightedtotal = sum [ scoreWeightBySpeed sp $ sum (map snd scs) | (sp, scs) <- highscoresbyspeed]
    scorew = length $ show (maximumDef 0 scores)
    width = length col1heading + maxcavenum * (scorew+1)
    col1heading = "   cave:"
  putStr col1heading
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
  printf ("%"++show width++"s\n") (printf "         Total: %7d" total :: String)
  printf ("%"++show width++"s\n") (printf "Speed-weighted: %7d" weightedtotal :: String)
  putStr "\n"
  putStr $ progressMessage sstate

scoreWeightBySpeed :: MaxSpeed -> Score -> Score
scoreWeightBySpeed speed score = round $ fromIntegral score ** (1 + weight)
  where
    neutralspeed  = 15  -- no weighting at this speed (= defmaxspeed)
    relativespeed = fromIntegral speed - neutralspeed
    weight        = relativespeed / weightdiv
    weightdiv     = 100 -- larger means less weight

printSampleWeightedScores = pprint $ groupSort $ reverse 
  [(sc, (sp, scoreWeightBySpeed sp sc)) | sp <- [5,10..60], sc <- [1,10,100,460]]

-- Print the current or selected cave to stdout, generating it just like the game would.
-- With a depth argument, print just the first N lines.
printCave cave mdepth SavedState{allcrashes} = do
  let crashes = fromMaybe M.empty $ M.lookup cave allcrashes
  putStrLnAnsi [bold_] $ progname ++ " cave "++show cave
  go mdepth $ newGameState False gamewidth 25 cave 15 0 crashes
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
          s = showCaveLineNumbered gamewidth l cavesteps'
          s' = foldl' drawcrash s rowcrashes
            where
              rowcrashes = maybe [] M.toList $ M.lookup cavesteps' cavecrashes
              drawcrash s (col,num) 
                | isSpace c = s
                | otherwise = take (col-1) s ++ [c] ++ drop col s
                where 
                  c = crashChar num
        putStrLn s'
        go (fmap (subtract 1) mremaining)
           g{randomgen    = randomgen'
            ,cavesteps    = cavesteps'
            ,cavelines    = cavelines'
            ,cavewidth    = cavewidth'
            ,cavecenter   = cavecenter'
            }

-- Play the game repeatedly at the given cave and speed,
-- updating save files and/or advancing to next cave when appropriate.
-- The first argument specifies if this is the first game of a session.
playGames :: Bool -> SavedState -> IO ()
playGames firstgame sstate@SavedState{..} = do
  (screenw,screenh) <- displaySize  -- use full screen height for each game (apparently last line is unusable on windows ? surely it's fine)
  let
    highscore = fromMaybe 0 $ M.lookup (currentspeed, currentcave) highscores
    crashes = fromMaybe M.empty $ M.lookup currentcave allcrashes
    game = newGame firstgame screenh currentspeed currentcave highscore crashes

  -- run one game. Will exit if terminal is too small.
  g@GameState{scene,score,exit,playerx} <- Terminal.Game.playGameS game
  -- game ended by crashing or quitting. Ctrl-c is not caught here.

  let atend = playerAtEnd g
  sstate' <- case scene of
    -- game ended by pressing q; no state changes
    Playing -> return sstate

    -- game ended by crashing at or before cave end
    RunEnd compl _ -> do
      -- persistent state: log a run end event
      t <- getCurrentTime 
      logAppend [(if compl then Compl else Crash) t currentspeed currentcave (playerDepth g) playerx score]
      -- in-memory state:
      let
        -- select next cave & speed
        (nextcave, nextspeed)
          | not compl                 = (currentcave,   currentspeed)
          | currentcave < maxcavenum  = (currentcave+1, currentspeed)
          | otherwise                 = (1,             currentspeed+5)
        -- update high score/high cave if appropriate (XXX needs next cave number, wrong ?)
        sstate1 = savedStateUpdate currentspeed currentcave score nextcave (playerDepth g) playerx sstate
      return sstate1{currentcave=nextcave, currentspeed=nextspeed}

  -- play again, or exit the app
  if not exit
  then playGames False sstate'
  else do
    putStr $ progressMessage sstate'
    putStrLn ""
    printScores
    when soundEnabled quitSound

-- Initialise a new game (a cave run).
newGame :: Bool -> Height -> MaxSpeed -> CaveNum -> Score -> CaveCrashes -> Game GameState
newGame firstgame gameh maxspeed cave hs crashes =
  Game {
     gTPS           = tps         -- target game ticks per second
    ,gInitState     = gstate      -- initial game state
    ,gLogicFunction = stepCommon  -- logic function
    ,gDrawFunction  = draw        -- drawing function
    ,gQuitFunction  = timeToQuit  -- time to quit function
  }
  where
    gstate = newGameState firstgame gamewidth gameh cave maxspeed hs crashes

-------------------------------------------------------------------------------
-- event handlers & game logic for each scene

-- Before calling step, do general event processing common to all modes.
stepCommon genv g@GameState{..} ev@(KeyPress k)
  | k == 'q'         = g { exit=True }
  | k == 's'         = g { showstats=not showstats }
  | k == 'p'         = g { pause=not pause }
  -- space is another pause key, except during run end "press a key" phase
  | k == ' ', scenePhase scene /= 3 = g { pause=not pause }
stepCommon genv g@GameState{..} Tick = step genv g' Tick
  where
    g' = g{
       gtick=gtick+1
      ,stick=(if pause then id else (+1)) stick
      ,showhelp=showhelp && not (timeToHideHelp g)
      }
stepCommon genv g ev = step genv g ev


step genv g@GameState{scene=Playing, ..} (KeyPress k)
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
step genv g@GameState{scene=RunEnd _ phase, ..} (KeyPress _)
  | phase==3, not pause = g{restart=True}
step _ g (KeyPress _) = g
step genv g@GameState{scene=Playing, ..} Tick =
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
        g{ scene        = RunEnd victory 1
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
step genv g@GameState{scene=RunEnd compl phase, ..} Tick
  -- run end + completionbonusdelaysecs: add score bonus, maybe show high score message, phase 2
  | phase==1 && stick > secsToTicks completionbonusdelaysecs =
      let
        bonus | compl     = round cavespeedmax * completionbonusmultiplier
              | otherwise = 0
        score' = score + bonus
      in
        (if score > highscore then unsafePlay endGameHighScoreSound else id) $
        g{scene=RunEnd compl 2
        ,restarttimer = tick restarttimer
        ,score      = score'
        ,scorebonus = bonus  -- save this for display
        ,highscore  = max highscore score'
        }
  -- run end + completionadvancedelaysecs: phase 3 (show press key to continue)
  | phase==2 && stick > secsToTicks completionadvancedelaysecs =
      g{scene=RunEnd compl 3
       ,restarttimer = tick restarttimer
       }
  | otherwise = g{restarttimer = tick restarttimer}

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
    cavelines' = take (gameh * 2) $ CaveLine cavesteps' l r : cavelines
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

gameOver GameState{scene=RunEnd _ _} = True
gameOver _ = False

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
    Nothing               -> False
    Just (CaveLine _ l r) -> playerx <= l || playerx > r

-- How close is player flying to a wall ?
-- Actually, checks the line below (ahead) of the player, to sync better with sound effects.
playerWallDistance :: GameState -> Maybe Width
playerWallDistance g@GameState{..} =
  case playerLineBelow g of
    Nothing               -> Nothing
    Just (CaveLine _ l r) -> Just $ min (max 0 $ playerx-l) (max 0 $ r+1-playerx)

-- Has player reached the cave bottom (a zero-width line) ?
playerAtEnd g = case playerLine g of
  Just (CaveLine _ l r) | r <= l -> True
  _                              -> False

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

displaySizeStrSafe = handle (\(_::ATGException) -> return "unknown") $ do
  (w,h) <- displaySize
  return $ show w++"x"++show h

-- Convert seconds to game ticks based on global frame rate.
secsToTicks :: Float -> Integer
secsToTicks = round . (* fromIntegral tps)

half :: Integral a => a -> a
half = (`div` 2)


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

insertOrUpdate :: Ord k => v -> (v -> v) -> k -> M.Map k v -> M.Map k v
insertOrUpdate newval updatefn = M.alter (maybe (Just newval) (Just . updatefn))

count :: Ord a => [a] -> [(a, Int)]
count xs = [(y, length ys) | ys@(y:_) <- group $ sort xs]

-- Exit with an error message and no stack trace, after resetting the
-- terminal style if stdout supports ANSI. Uses unsafePerformIO.
err = unsafePlay (setSGR' []) . errorWithoutStackTrace

-- Set stdout's terminal output style (Select Graphics Rendition mode)
-- if it supports ANSI color.
setSGR' sgrs = hSupportsANSIColor stdout >>= flip when (setSGR sgrs)

-- Set stdout's terminal output style before printing, then reset it,
-- if it supports ANSI color.
putStrLnAnsi sgrs s = setSGR' sgrs >> putStrLn s >> setSGR' []
putStrAnsi   sgrs s = setSGR' sgrs >> putStr   s >> setSGR' []

-- https://hackage.haskell.org/package/ansi-terminal/docs/System-Console-ANSI-Types.html
bold_           = SetConsoleIntensity BoldIntensity
normal          = SetConsoleIntensity NormalIntensity
reverse_        = SetSwapForegroundBackground True
forward         = SetSwapForegroundBackground False
fgblack         = SetColor Foreground Dull Black
fgred           = SetColor Foreground Dull Red
fggreen         = SetColor Foreground Dull Green
fgyellow        = SetColor Foreground Dull Yellow
fgblue          = SetColor Foreground Dull Blue
fgmagenta       = SetColor Foreground Dull Magenta
fgcyan          = SetColor Foreground Dull Cyan
fgwhite         = SetColor Foreground Dull White
fgvividblack    = SetColor Foreground Vivid Black
fgvividred      = SetColor Foreground Vivid Red
fgvividgreen    = SetColor Foreground Vivid Green
fgvividyellow   = SetColor Foreground Vivid Yellow
fgvividblue     = SetColor Foreground Vivid Blue
fgvividmagenta  = SetColor Foreground Vivid Magenta
fgvividcyan     = SetColor Foreground Vivid Cyan
fgvividwhite    = SetColor Foreground Vivid White
bgblack         = SetColor Background Dull Black
bgred           = SetColor Background Dull Red
bggreen         = SetColor Background Dull Green
bgyellow        = SetColor Background Dull Yellow
bgblue          = SetColor Background Dull Blue
bgmagenta       = SetColor Background Dull Magenta
bgcyan          = SetColor Background Dull Cyan
bgwhite         = SetColor Background Dull White
bgvividblack    = SetColor Background Vivid Black
bgvividred      = SetColor Background Vivid Red
bgvividgreen    = SetColor Background Vivid Green
bgvividyellow   = SetColor Background Vivid Yellow
bgvividblue     = SetColor Background Vivid Blue
bgvividmagenta  = SetColor Background Vivid Magenta
bgvividcyan     = SetColor Background Vivid Cyan
bgvividwhite    = SetColor Background Vivid White

-------------------------------------------------------------------------------
-- drawing for each scene

blankPlaneFull GEnv{eTermDims=(termw,termh)} = blankPlane termw termh

centered genv = (blankPlaneFull genv ***)

draw genv@GEnv{eTermDims=(termw,termh),..} g@GameState{gamew,gameh,..} =
  -- clear screen, center game drawing area
    centered genv $ blankPlane gamew gameh
  -- cave
  & (max 1 (gameh - length cavelines + 1), 1) % drawCave g
  -- top info
  & (1, titlex)     % drawTitle g
  & (1, cavenamex)  % drawCaveName g
  & (1, highscorex) % drawHighScore g
  & (1, scorex)     % drawScore g
  & (1, speedx)     % drawSpeed g
  -- help
  & (if showhelp || pause then (3, helpx) % drawHelp g else id)
  -- stats
  & (if showstats then (3, gamew - 13) % drawStats genv g else id)
  -- run end dialog
  & (if gameOver g then (gameovery, gameoverx) % drawGameOver g gameoverw gameoverh else id)
  -- player
  & (playery+speedpan, playerx) % drawPlayer g
  where
    titlew     = 12
    cavenamew  = fromIntegral $ 10 + length (show cavenum) + length (show cavespeedmax)
    highscorew = 17
    scorew     = 11
    speedw     = 10
    gameoverw  = 48
    gameoverh  = 7

    titlex     = 1
    cavenamex  = min (highscorex - cavenamew) (half (highscorex - (titlex+titlew)) + titlex + titlew - half cavenamew)
    highscorex = half gamew - half highscorew
    scorex     = highscorex + highscorew + half (speedx - (highscorex + highscorew)) - half scorew
    speedx     = gamew - speedw + 1
    helpx      = 1
    gameoverx  = half gamew - half gameoverw
    gameovery  = max (playery+2) $ half gameh - half gameoverh

-------------------------------------------------------------------------------
-- drawing helpers

drawCave GameState{..} =
  vcat $
  map (drawCaveLine gamew cavecrashes) $
  reverse $
  take gameh $
  drop speedpan cavelines

drawCaveLine gamew cavecrashes line@(CaveLine d _ _) = 
  mergePlanes
    (stringPlane $ showCaveLine gamew line)
    [((1,col), drawCrash num) | (col,num) <- crashes]
  where
    crashes :: [(CaveCol, Int)]
    crashes = maybe [] M.toList $ M.lookup d cavecrashes

drawCrash num = 
  stringPlaneTrans ' ' [crashChar num] #bold #invert -- #color hue Vivid 

crashChar :: Int -> Char
crashChar n 
  | n < 1     = ' '
  | n == 1    = '*'
  | n <= 9    = chr $ ord '0' + n
  | otherwise = 'X'

showCaveLine gamew (CaveLine depth left right) =
  concat [
    replicate left wallchar
   ,replicate (right - left) spacechar
   ,replicate (gamew - right ) wallchar
   ]

showCaveLineNumbered gamew l n =
  num ++ drop (length num) (showCaveLine gamew l  )
  where
    num = show n

drawPlayer g@GameState{..} =
  cell char #bold #color hue Vivid #maybeinvert
  where
    (char, hue, maybeinvert) = 
      case scene of 
        RunEnd False _ -> (crashChar numcrashes, Red, invert)
        _              -> (playerchar,           Blue, id)
        where
          numcrashes = 1 + caveCrashesAt (playerDepth g) playerx cavecrashes

drawTitle GameState{..} =
  hcat $ zipWith (\a b -> a b) colors (map (bold.cell) $ progname++"! ")
  where 
    colors = drop (cavesteps `div` 3 `mod` 4) $ cycle colorsRGBY
    colorsRGBY = [color Red Vivid, color Green Vivid, color Blue Vivid, color Yellow Vivid]

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

drawStats GEnv{..} g@GameState{..} =
      (stringPlane "     fps " ||| stringPlane (printf "%4d " eFPS))
  === (stringPlane "   gtick " ||| stringPlane (printf "%4d " gtick))
  === (stringPlane "   stick " ||| stringPlane (printf "%4d " stick))
  === (stringPlane "scene " ||| stringPlane (printf "%-7s " (showSceneCompact 7 scene)))
  === (stringPlane "    depth " ||| stringPlane (printf "%3d " (playerDepth g)))
  -- === (stringPlane "depthmod5 " ||| stringPlane (printf "%3d " (playerDepth g `mod` 5)))
  === (stringPlane "    width " ||| stringPlane (printf "%3d " cavewidth))
  === (stringPlane " minspeed " ||| stringPlane (printf "%3.f " cavespeedmin))
  -- === (stringPlane " speedpan " ||| stringPlane (printf "%3d " speedpan))
  -- === (stringPlane "    speed " ||| stringPlane (printf "%3.f " cavespeed))
  -- === (stringPlane "   bonus " ||| stringPlane (printf "%4d " scorebonus))
  === (stringPlane "   score " ||| stringPlane (printf "%4d " score))
  === (stringPlane "  hscore " ||| stringPlane (printf "%4d " highscore))
  === (stringPlane "hscorecp " ||| stringPlane (printf "%4d " highscorecopy))

showSceneCompact width scene = 
  case scene of
    RunEnd _ n -> take (width-length sn) ss ++ sn
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
      RunEnd False _ -> [
         "GAME OVER"
        ,if showline2 then printf "You reached depth %d.%s" (playerDepth g) hs else ""
        ,if showline3 then "Press a key to relaunch.." else ""
        ]
      RunEnd True _ -> [
         printf "CAVE %d COMPLETE" cavenum
        ,if showline2 then printf "Nice flying! Bonus: %d.%s" scorebonus hs else ""
        ,if showline3 then "Press a key to advance.." else ""
        ]
      _ -> ["","",""]
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
