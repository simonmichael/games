#!/usr/bin/env stack script --compile --resolver lts-18 --verbosity=warn
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}

-- ski.hs - Downhill Skier Driver Space Pilot !

import Control.Concurrent
import Control.Monad
-- import Data.Char
-- import Data.List
import Debug.Trace
-- import System.Environment
import System.IO
import System.Random
import Text.Printf

data GameState = GameState {
   score       :: Int
  ,wallchar    :: Char
  ,delay       :: Int
  ,screenwidth :: Int
  ,pathchar    :: Char
  ,pathwidth   :: Int
  ,pathcenter  :: Int
  }

mkGamestate w = GameState {
   score       = 0
  ,wallchar    = '#'
  ,delay       = delayinit
  ,screenwidth = w
  ,pathchar    = ' '
  ,pathwidth   = pathwidthinit
  ,pathcenter  = w `div` 2
  }

delayinit = 200000
pathwidthinit = 40
pathwidthmin = 0

main = do
  setup
  intro
  loop $ mkGamestate 80

setup = do
  hSetEcho stdout False
  hSetBuffering stdout NoBuffering
  -- putStr "\033[2J"

intro = do
  let delay = 1000000
  putStrLn ""
  putStrLn "** Welcome, Downhill Skier Driver Space Pilot! How far can you go ? **"
  threadDelay delay
  putStrLn "Your mouse pointer is you. Avoid the walls! Honour system collision detection."
  threadDelay delay
  putStrLn "** Get ready to race! **"
  putStrLn ""
  threadDelay delay

loop g@GameState{..} = do
  -- calculate
  let
    score'     = score + 1
    margin     = 8
    pathmin    = margin
    pathmax    = screenwidth - margin
    scorediv10 = score `div` 10 + 1
    delay'     = max 20000 (delayinit - scorediv10 * 15000)
    pathwidth' = max pathwidthmin (pathwidthinit - scorediv10)
    maxdx      = min (pathwidth' `div` 3) scorediv10
  dx <- randomRIO (-maxdx,maxdx)
  let
    pathLeft  center width = center - half width
    pathRight center width = center + half width
    pathcenter' =
      case pathcenter + dx of
        x | pathLeft  x pathwidth' < pathmin -> pathmin + half pathwidth'
        x | pathRight x pathwidth' > pathmax -> pathmax - half pathwidth'
        x -> x

  -- draw
  let
    leftwallwidth = pathLeft pathcenter' pathwidth' - 1
    rightwallwidth = screenwidth - pathRight pathcenter' pathwidth' - 1
  -- print [
  --    leftwallwidth
  --   ,pathwidth
  --   ,rightwallwidth
  --   ]
  putStrLn $ concat [
     replicate leftwallwidth wallchar
    ,replicate pathwidth pathchar
    ,replicate rightwallwidth wallchar
    ,"\r"
    ,printf "# %4d " score'
    ]

  -- loop
  threadDelay delay
  when (pathwidth > 0) $
    loop g{score=score', delay=delay', pathcenter=pathcenter', pathwidth=pathwidth'}

half = (`div` 2)
