#!/usr/bin/env stack 
-- stack --resolver=lts-18 script --optimize --verbosity=warn --ghc-options=-threaded --package random --package ansi-terminal-game --package linebreak --package timers-tick --package unidecode --package safe
-------------------------------------------------------------------------------

-- cave2.hs - a one file haskell terminal game, using ansi-terminal-game.
-- 
-- stack is not required to run or compile this haskell script, but it
-- makes things just work. On first running this script it may hang
-- for a while (could be minutes) to install ghc and any required
-- packages. Change "warn" above to "info" to see progress output.

-- Setting keyboard repeat rate:
-- Mac:
-- https://apple.stackexchange.com/questions/411531/increase-keyrepeat-speed-in-macos-big-sur
--  defaults write NSGlobalDomain InitialKeyRepeat -int 10  # 15
--  defaults write NSGlobalDomain KeyRepeat -int 1          # 2

-------------------------------------------------------------------------------

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-imports #-}
{-# LANGUAGE MultiWayIf, RecordWildCards #-}

import Control.Monad
import Debug.Trace
import Safe
import Terminal.Game
import Text.Printf

-------------------------------------------------------------------------------

(leftkey,rightkey) = (',','.')
restarttimerticks  = secsToTicks 5
wallchar       = '#'
pathchar       = ' '
crashchar      = '*'
fps            = 60

pathmarginmin  = 2
pathwidthmin   = 0

pathspeedmax   = float $ half fps -- maximum speed, should be <= fps

-- pathspeedinit  = 2               -- initial path scrolling speed
-- pathspeedaccel = 1.005           -- multiply speed by this much each game tick
-- pathspeedbrake = 0.7             -- multiply speed by this much each player movement (autobraking)
-- -- player's minimum/maximum y position relative to screen height.
-- -- if different, view panning is enabled.
-- (playerymin, playerymax) = (0.2, 0.6)

pathspeedinit  = 4
pathspeedaccel = 1.001
pathspeedbrake = 0.8
(playerymin, playerymax) = (0.4, 0.4)

-- pathspeedinit  = 1
-- pathspeedaccel = 1.01
-- pathspeedbrake = 0.9
-- (playerymin, playerymax) = (0.1, 0.7)

-------------------------------------------------------------------------------

data GameState = GameState {
   screenw         :: Width
  ,screenh         :: Height
  ,randomgen       :: StdGen
  ,gtick           :: Integer    -- current game tick
  ,highscore       :: Integer
  ,score           :: Integer
  ,pathsteps       :: Integer    -- how many path segments have been traversed since game start
  ,path            :: [PathLine] -- path segments, newest/bottom-most first
  ,pathwidth       :: Width      -- current path width
  ,pathcenter      :: Column     -- current path center
  ,pathspeed       :: Float      -- current speed of path scroll in steps/s, must be <= fps
  ,pathspeedbase   :: Float      -- current minimum path scroll speed player can brake to
  ,pathtimer       :: Timed Bool -- delay before next path scroll
  ,viewscroll      :: Height     -- how many rows to pan the viewport down, based on current speed
  ,playery         :: Row
  ,playerx         :: Column
  ,playerchar      :: Char
  ,playercollision :: Bool       -- player has crashed
  ,restarttimer    :: Timed Bool -- delay between player collision and restart
  ,pause           :: Bool       -- whether to keep the game paused
  ,exit            :: Bool       -- whether to completely exit the app
  }

newGameState w h rg hs = GameState {
   screenw         = w
  ,screenh         = h
  ,randomgen       = rg
  ,gtick           = 0
  ,highscore       = hs
  ,score           = 0
  ,pathsteps       = 0
  ,path            = []
  ,pathwidth       = half w
  ,pathcenter      = half w
  ,pathspeed       = pathspeedinit
  ,pathspeedbase   = pathspeedinit * 2
  ,pathtimer       = newPathTimer pathspeedinit
  ,viewscroll      = 0
  ,playery         = playerYMin h
  ,playerx         = half w
  ,playerchar      = 'V'
  ,playercollision = False
  ,restarttimer    = creaBoolTimer 0
  ,pause           = False
  ,exit            = False
  }

data PathLine = PathLine Column Column  -- left wall, right wall

-------------------------------------------------------------------------------

main = do
  (w,h) <- displaySize
  rg <- getStdGen
  playloop w h rg 0

playloop w h rg hs = do
  GameState{..} <- playGameS $ newGame w h rg hs
  when (not exit) $ do
    (w',h') <- displaySize
    playloop w' h' randomgen (max highscore score)

newGame screenw screenh rg hs = 
  Game { gScreenWidth   = screenw,
         gScreenHeight  = screenh-1,  -- last line is unusable on windows apparently
         gFPS           = fps,
         gInitState     = newGameState screenw screenh rg hs,
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
        , pathspeed = max pathspeedbase (pathspeed * pathspeedbrake)
        }
  | k == rightkey, not (playercollision || pause) =
      g { playerx = min screenw (playerx + 1)
        , pathspeed = max pathspeedbase (pathspeed * pathspeedbrake)
        }
step g@GameState{..} Tick =
  let
    -- game
    gtick'      = gtick + 1
    gtick10     = gtick' `div` 10 + 1

    -- path
    -- gradually narrow path
    pathwidth' = max pathwidthmin (half screenw - pathsteps `div` 10)
    -- gradually accelerate
    pathspeed' = min pathspeedmax (pathspeed * pathspeedaccel)
    -- slowly increase minimum speed ?
    -- pathspeedbase' = pathspeedinit * 2 + float (pathsteps `div` 100)
    pathspeedbase' = pathspeedbase

    -- player
    playercollision' =
      case path `atMay` int (playerHeight g) of
        Just (PathLine l r) -> playerx <= l || playerx > r
        Nothing             -> False
    
    -- start restart timer if collision just happened
    restarttimer' =
      case (playercollision, playercollision') of
        (False, True) -> creaBoolTimer restarttimerticks
        _             -> tick restarttimer
                          
    -- update things depending on the situation
    (rg', pathcenter', path', pathsteps', pathtimer', score', highscore', viewscroll')
      | playercollision' =
        -- player has crashed
        (randomgen, pathcenter, path, pathsteps, pathtimer, score, max score highscore, viewscroll)
      | isExpired pathtimer =
        -- scroll the path
       -- trace "scroll" $
        let
          -- choose path's next sideways displacement
          -- maxdx = 1
          maxdx = min (pathwidth' `div` 4) (pathsteps `div` 100 + 1)
          (pathdx, rg') = getRandom (-maxdx,maxdx) randomgen
          -- adjust the path center sideways within limits
          pathcenter' =
            case pathcenter + pathdx of
              x | pathLeft  x pathwidth' < pathmin -> pathmin + half pathwidth'
              x | pathRight x pathwidth' > pathmax -> pathmax - half pathwidth'
              x -> x
            where
              pathLeft  center width = center - half width
              pathRight center width = center + half width
              pathmargin = max pathmarginmin (screenw `div` 40)
              pathmin    = pathmargin
              pathmax    = screenw - pathmargin
          l = pathcenter' - half pathwidth'
          r = pathcenter' + half pathwidth'
          -- Pan the viewport up (player and walls move down) as speed increases,
          -- as much as player Y min and max allow,
          -- at most one row every few path steps, and only after screen has filled.
          vs' =
            if | length path < int screenh               -> viewscroll
               | length path < int screenh               -> viewscroll
               | vs > viewscroll, pathsteps `mod` 5 == 0 -> viewscroll+1
               | vs < viewscroll, pathsteps `mod` 5 == 0 -> viewscroll-1
               | otherwise                               -> viewscroll
            where
              vs = round $
                   float (playerYMax screenh - playerYMin screenh)
                   * (pathspeed'-pathspeedinit) / (pathspeedmax-pathspeedinit)
        in
          (rg'
          ,pathcenter'
          ,take (int screenh * 2) $ PathLine l r : path  -- extra offscreen lines are saved for viewscroll
          ,pathsteps + 1
          ,newPathTimer pathspeed'
          ,score + if pathsteps >= playerHeight g then 1 else 0
          ,highscore
          ,vs'
          )
     | otherwise =
       -- wait
       -- trace "wait" $
       (randomgen, pathcenter, path, pathsteps, tick pathtimer, score, highscore, viewscroll)

  in
    if | pause || playercollision ->
         g{gtick           = gtick'
          ,restarttimer    = restarttimer'
          }
       | otherwise ->
         g{randomgen       = rg'
          ,gtick           = gtick'
          ,highscore       = highscore'
          ,score           = score'
          ,pathsteps       = pathsteps'
          ,path            = path'
          ,pathcenter      = pathcenter'
          ,pathwidth       = pathwidth'
          ,pathspeed       = pathspeed'
          ,pathspeedbase   = pathspeedbase'
          ,pathtimer       = pathtimer'
          ,viewscroll      = viewscroll'
          ,playercollision = playercollision'
          ,restarttimer    = restarttimer'
          }
step g _ = g

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
  & (1,1) % drawTitle
  & (3,1) % drawHelp g
  & (1, half screenw - 8) % drawHighScore highscore
  & (1, screenw - 10) % drawScore score
  & (2, screenw - 10) % drawSpeed g
  -- & (3, screenw - 13) % drawStats g
  & (playery+viewscroll, playerx) % cell c #bold #color hue Vivid
  where
    (c,hue) | playercollision = (crashchar,Red)
            | otherwise       = (playerchar,Blue)

drawTitle = hcat [
   cell 'c' #bold #color Red Vivid
  ,cell 'a' #bold #color Blue Vivid
  ,cell 'v' #bold #color Yellow Vivid
  ,cell 'e' #bold #color Green Vivid
  ,cell '-' #bold #color Red Vivid
  ,cell 'r' #bold #color Blue Vivid
  ,cell 'u' #bold #color Yellow Vivid
  ,cell 'n' #bold #color Green Vivid
  ,cell 'n' #bold #color Red Vivid
  ,cell 'e' #bold #color Blue Vivid
  ,cell 'r' #bold #color Yellow Vivid
  ,cell '!' #bold #color Green Vivid
  ,cell ' '
  ]

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
  === (stringPlane " minspeed " ||| stringPlane (printf "%3.f " pathspeedbase))
  -- === (stringPlane " speedpan " ||| stringPlane (printf "%3d " viewscroll))
  -- === (stringPlane "    speed " ||| stringPlane (printf "%3.f " pathspeed))

drawPath GameState{..} = vcat (map (drawPathLine screenw) $ reverse $ take (int screenh) $ drop (int viewscroll) path)

drawPathLine screenw (PathLine left right) = stringPlane line
  where
    line = concat [
       replicate (int left) wallchar
      ,replicate (int $ right - left) pathchar
      ,replicate (int $ screenw - right ) wallchar
      ]

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
playerHeight GameState{..} = screenh - playery - 1

-- Calculate the player's minimum and maximum y coordinate.
playerYMin screenh = round $ playerymin * float screenh
playerYMax screenh = round $ playerymax * float screenh

half :: Integral a => a -> a
half = (`div` 2)

float :: Integer -> Float
float = fromInteger

int :: Integer -> Int
int = fromIntegral
