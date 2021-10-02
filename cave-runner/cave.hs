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
{-# LANGUAGE MultiWayIf, NamedFieldPuns, RecordWildCards #-}

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
  ,speedpan        :: Height     -- current number of rows to pan the viewport down, based on current speed
  ,pathsteps       :: Integer    -- how many path segments have been traversed since game start
  ,path            :: [PathLine] -- recent path segments for display, newest/bottom-most first
  ,pathwidth       :: Width      -- current path width
  ,pathcenter      :: Column     -- current path center
  ,pathspeed       :: Float      -- current speed of path scroll in steps/s, must be <= fps
  ,pathspeedbase   :: Float      -- current minimum path scroll speed player can brake to
  ,pathtimer       :: Timed Bool -- delay before next path scroll
  ,playery         :: Row
  ,playerx         :: Column
  ,playerchar      :: Char
  ,playercollision :: Bool       -- player has crashed ?
  ,restarttimer    :: Timed Bool -- delay before restart after player crash
  ,pause           :: Bool       -- keep the game paused ?
  ,exit            :: Bool       -- completely exit the app ?
  }

newGameState w h rg hs = GameState {
   screenw         = w
  ,screenh         = h
  ,randomgen       = rg
  ,gtick           = 0
  ,highscore       = hs
  ,score           = 0
  ,speedpan        = 0
  ,pathsteps       = 0
  ,path            = []
  ,pathwidth       = half w
  ,pathcenter      = half w
  ,pathspeed       = pathspeedinit
  ,pathspeedbase   = pathspeedinit * 2
  ,pathtimer       = newPathTimer pathspeedinit
  ,playery         = playerYMin h
  ,playerx         = half w
  ,playerchar      = 'V'
  ,playercollision = False
  ,restarttimer    = creaBoolTimer restarttimerticks
  ,pause           = False
  ,exit            = False
  }

data PathLine = PathLine Column Column  -- left wall, right wall

-------------------------------------------------------------------------------

main = do
  (w,h) <- displaySize
  let caveseed = 1
  playloop w h (mkStdGen caveseed) 0

playloop w h rg hs = do
  GameState{randomgen,score,highscore,exit} <- playGameS $ newGame w h rg hs
  unless exit $ do
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
    -- gravity - gradually accelerate
    pathspeed' = min pathspeedmax (pathspeed * pathspeedaccel)

    g' = g{gtick     = gtick+1
          ,pathspeed = pathspeed'
          }

    -- has player crashed ?
    playercollision' =
      case path `atMay` int (playerHeight g) of
        Nothing             -> False
        Just (PathLine l r) -> playerx <= l || playerx > r

  in  -- breaks my haskell-mode's indentation
    if
      | pause ->  -- paused
        g'

      | not playercollision && playercollision' ->  -- newly crashed
        g'{playercollision = True
          ,highscore       = max score highscore
          ,restarttimer    = reset restarttimer
          }

      | playercollision ->  -- previously crashed, awaiting restart
        g'{restarttimer = tick restarttimer}

      | isExpired pathtimer ->  -- time to step the path
        let
          (pathsteps',
           pathspeedbase',
           pathwidth',
           randomgen',
           pathcenter',
           path')   = stepPath     g'
          speedpan' = stepSpeedpan g' pathsteps' pathspeed' path'
          score'    = stepScore    g' pathsteps'
        in
          g'{randomgen       = randomgen'
            ,score           = score'
            ,speedpan        = speedpan'
            ,pathsteps       = pathsteps'
            ,path            = path'
            ,pathwidth       = pathwidth'
            ,pathcenter      = pathcenter'
            ,pathspeed       = pathspeed'
            ,pathspeedbase   = pathspeedbase'
            ,pathtimer       = newPathTimer pathspeed'
            ,playercollision = playercollision'
            }

      | otherwise ->  -- time is passing
        g'{pathtimer = tick pathtimer}

stepPath GameState{..} =
  (pathsteps'
  ,pathspeedbase'
  ,pathwidth'
  ,randomgen'
  ,pathcenter'
  ,path')
  where
    pathsteps' = pathsteps + 1

    -- hurryup - slowly increase minimum speed ?
    -- pathspeedbase' = pathspeedinit * 2 + float (pathsteps `div` 100)
    pathspeedbase' = pathspeedbase

    -- narrowing - gradually narrow path
    pathwidth' = max pathwidthmin (half screenw - pathsteps' `div` 10)

    -- morejagged - slowly increase max allowed sideways shift
    -- maxdx = 1
    maxdx = min (pathwidth' `div` 4) (pathsteps' `div` 100 + 1)

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
  & (1,1) % drawTitle
  & (3,1) % drawHelp g
  & (1, half screenw - 8) % drawHighScore highscore
  & (1, screenw - 10) % drawScore score
  & (2, screenw - 10) % drawSpeed g
  -- & (3, screenw - 13) % drawStats g
  & (playery+speedpan, playerx) % cell c #bold #color hue Vivid
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
