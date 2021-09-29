#!/usr/bin/env stack 
-- stack script --compile --resolver=lts-18 --verbosity=warn
--   --package random
--
-- ski.hs - Downhill Skier Driver Space Pilot !
-- 
-- stack is not required to run or compile this script, but it makes
-- things just work. The first time you run this script, it may
-- install ghc and any required packages, causing it to hang for a
-- while with no output. Change "warn" above to "info" to see the
-- output, or just wait.

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent
import Control.Monad
import Debug.Trace
import System.IO
import System.Random
import Text.Printf
import System.Exit

delayinit = 200000
pathwidthinit = 60
pathwidthmin = 0
crashchar = 'x'

data GameState = GameState {
   score       :: Int
  ,wallchar    :: Char
  ,delay       :: Int
  ,screenwidth :: Int
  ,pathchar    :: Char
  ,pathwidth   :: Int
  ,pathcenter  :: Int
  ,playerx     :: Int
  ,playerchar  :: Char
  }

mkGamestate w = GameState {
   score       = 0
  ,wallchar    = '#'
  ,delay       = delayinit
  ,screenwidth = w
  ,pathchar    = ' '
  ,pathwidth   = pathwidthinit
  ,pathcenter  = w `div` 2
  ,playerx     = w `div` 2
  ,playerchar  = 'V'
  }

main = do
  setup
  intro
  loop $ mkGamestate 80

setup = do
  hSetEcho stdout False
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering

intro = do
  let delay = 1000000
  putStrLn "** Welcome to the Downhill Skier Driver Space Pilot Simulator! **"
  -- threadDelay delay
  -- putStrLn "Your mouse pointer is you. Avoid the walls! Honour system collision detection."
  -- threadDelay delay
  -- putStrLn "Get ready to race!"
  -- putStrLn "How far will the bot get ?"
  putStrLn ""
  threadDelay delay

loop g@GameState{..} = do
  -- calculate
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
  pathdx <- randomRIO (-maxdx,maxdx)
  playerdx <- randomRIO (-1,1)
  let
    pathLeft  center width = center - half width
    pathRight center width = center + half width
    pathcenter' =
      case pathcenter + pathdx of
        x | pathLeft  x pathwidth' < pathmin -> pathmin + half pathwidth'
        x | pathRight x pathwidth' > pathmax -> pathmax - half pathwidth'
        x -> x
    -- playerx'   = case input of
    --                'z' -> playerx - 1
    --                'x' -> playerx + 1
    --                _   -> playerx
    playerx' = playerx + playerdx
    collision = abs (pathcenter' - playerx') > half pathwidth'
    g' = g{score=score'
          ,delay=delay'
          ,pathcenter=pathcenter'
          ,pathwidth=pathwidth'
          ,playerx=playerx'
          }

  -- draw
  let
    leftwallwidth  = pathLeft pathcenter' pathwidth' - 1
    rightwallwidth = screenwidth - leftwallwidth - pathwidth' - if collision then 1 else 0
    leftpathwidth  = playerx' - leftwallwidth - 1
    rightpathwidth = pathwidth' - leftpathwidth - 1
    line =
      take (screenwidth-4) (
        concat [
          replicate leftwallwidth wallchar
         ,replicate leftpathwidth pathchar
         ,[if collision then crashchar else playerchar]
         ,replicate rightpathwidth pathchar
         ,replicate (rightwallwidth-3) wallchar
         ]) ++
      printf "%4d" score'
    -- ,' ':show [leftwallwidth,pathwidth',rightwallwidth,sum [leftwallwidth,pathwidth',rightwallwidth]]
  putStrLn line

  -- loop
  if collision
  then do
    putStrLn ""
    putStrLn "** BOOM! **"
    putStrLn $ "Score was " ++ show score' ++ "." -- , try again!"
    putStrLn "Press q to quit, any other key for another run."
    putStrLn ""
    c <- getChar
    case c of
      'q' -> exitSuccess
      _ -> do
        loop $ mkGamestate 80
  else do
    threadDelay delay
    loop g'
    
half = (`div` 2)
