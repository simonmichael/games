#!/usr/bin/env stack 
-- stack --resolver=lts-18 script --compile --verbosity=warn --ghc-options=-threaded --package random --package ansi-terminal-game --package linebreak --package timers-tick --package unidecode
--
-- ski.hs - a one file haskell terminal game.
-- 
-- stack is not required to run or compile this haskell script, but it
-- makes things just work. On first running this script it may hang
-- for a while (could be minutes) to install ghc and any required
-- packages. Change "warn" above to "info" to see progress output.

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-imports #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent
-- import Control.Monad
import Debug.Trace
-- import System.IO
import System.Random
import Terminal.Game
import Text.Printf
-- import System.Exit

delayinit     = 100000
pathwidthinit = 60
pathwidthmin  = 0
crashchar     = '*'

data GameState = GameState {
   score        :: Integer
  ,wallchar     :: Char
  ,delay        :: Int
  ,screenwidth  :: Width
  ,screenheight :: Height
  ,pathchar     :: Char
  ,pathwidth    :: Width
  ,pathcenter   :: Column
  ,playerx      :: Column
  ,playerchar   :: Char
  ,playercollision :: Bool
  ,quitpressed  :: Bool
  }

mkGamestate w h = GameState {
   score        = 0
  ,wallchar     = '#'
  ,delay        = delayinit
  ,screenwidth  = w
  ,screenheight = h
  ,pathchar     = ' '
  ,pathwidth    = pathwidthinit
  ,pathcenter   = w `div` 2
  ,playerx      = w `div` 2
  ,playerchar   = 'V'
  ,playercollision = False
  ,quitpressed  = False
  }

main = do
  (w,h) <- displaySize
  intro
  playGame
    Game { gScreenWidth   = w,
           gScreenHeight  = h,
           gFPS           = 13,
           gInitState     = mkGamestate w h,
           gLogicFunction = step,
           gDrawFunction  = draw,
           gQuitFunction  = quit
         }

intro = do
  -- putStrLn "** Welcome to the Downhill Skier Driver Space Pilot Simulator! **"
  -- putStrLn "Get ready to race!"
  -- threadDelay 1000000
  return ()

quit g@GameState{..} = quitpressed

step g@GameState{..} (KeyPress 'q') = g { quitpressed = True }
step g@GameState{..} (KeyPress ',') = g { playerx = max 1 (playerx - 1) }
step g@GameState{..} (KeyPress '.') = g { playerx = min screenwidth (playerx + 1) }
step g@GameState{..} Tick =
  let
    score'     = score + 1
    margin     = 8
    pathmin    = margin
    pathmax    = screenwidth - margin
    scorediv5  = score `div` 5 + 1
    scorediv10 = score `div` 10 + 1
    scorediv20 = score `div` 20 + 1
    -- delay'     = max 10000 (delayinit - scorediv5 * 25000)
    delay'     = max 10000 (delay - (delay `div` 50))
    pathwidth' = max pathwidthmin (pathwidthinit - scorediv10)
    maxdx      = 1 -- min (pathwidth' `div` 4) scorediv10
  -- pathdx <- randomRIO (-maxdx,maxdx)
    pathdx = 0 -- XXX
    pathcenter' =
      case pathcenter + pathdx of
        x | pathLeft  x pathwidth' < pathmin -> pathmin + half pathwidth'
        x | pathRight x pathwidth' > pathmax -> pathmax - half pathwidth'
        x -> x
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
    playercollision' = abs (pathcenter' - playerx) > half pathwidth'

  -- if playercollision
  -- then do
  --   putStrLn ""
  --   putStrLn "** BOOM! **"
  --   putStrLn $ "Score was " ++ show score ++ "." -- , try again!"
  --   putStrLn "Press q to quit, any other key for another run."
  --   putStrLn ""
    
  in
    g{score=score'
     ,delay=delay'
     ,pathcenter=pathcenter'
     ,pathwidth=pathwidth'
     ,playercollision=playercollision'
     }
step g _ = g

draw g@GameState{..} =
  let
    leftwallwidth  = pathLeft pathcenter pathwidth - 1
    rightwallwidth = screenwidth - leftwallwidth - pathwidth - if playercollision then 1 else 0
    leftpathwidth  = playerx - leftwallwidth - 1
    rightpathwidth = pathwidth - leftpathwidth - 1
    line =
      take (fromIntegral $ screenwidth-4) (
        concat [
          replicate (fromIntegral leftwallwidth) wallchar
         ,replicate (fromIntegral leftpathwidth) pathchar
         ,[if playercollision then crashchar else playerchar]
         ,replicate (fromIntegral rightpathwidth) pathchar
         ,replicate (fromIntegral $ rightwallwidth-3) wallchar
         ]) ++
      printf "%4d" score
    -- ,' ':show [leftwallwidth,pathwidth',rightwallwidth,sum [leftwallwidth,pathwidth',rightwallwidth]]
  in
    blankPlane screenwidth screenheight &
    (1, 1)  % box '#'    screenwidth     screenheight            &
    (2, 2)  % box ' '    (screenwidth-2) (screenheight-2)        &
    (1, 10) % textBox " , to move left, . to move right. Avoid the walls! q to quit " 61 1 &
    (half screenheight, playerx) % cell playerchar # color Green Vivid 

pathLeft  center width = center - half width

pathRight center width = center + half width

half = (`div` 2)
