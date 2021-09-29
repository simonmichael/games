#!/usr/bin/env stack 
-- stack script --compile --resolver=lts-18 --verbosity=warn
--   --package random

-- ski.hs - Downhill Skier Driver Space Pilot !

{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-imports #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Concurrent
import Control.Monad
import Debug.Trace
import System.IO
import System.Random
import Text.Printf
import System.Exit

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
  hSetBuffering stdin NoBuffering
  -- putStr "\033[2J"

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
  let input = ' '  -- input <- getChar
  let
    score'     = score + 1
    margin     = 8
    pathmin    = margin
    pathmax    = screenwidth - margin
    scorediv5  = score `div` 5 + 1
    scorediv10 = score `div` 10 + 1
    scorediv20 = score `div` 20 + 1
    -- delay'     = max 10000 (delayinit - scorediv5 * 25000)
    delay'     = max 5000 (delay - (delay `div` 50))
    pathwidth' = max pathwidthmin (pathwidthinit - scorediv20)
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
    collision = abs (pathcenter' - playerx') >= half pathwidth'
    g' = g{score=score'
          ,delay=delay'
          ,pathcenter=pathcenter'
          ,pathwidth=pathwidth'
          ,playerx=playerx'
          }

  -- draw
  let
    leftwallwidth  = pathLeft pathcenter' pathwidth' - 1
    rightwallwidth = screenwidth - pathRight pathcenter' pathwidth' - 1
    leftpathwidth  = playerx' - leftwallwidth
    rightpathwidth = pathwidth' - leftpathwidth - 1
  putStrLn $ concat [
     replicate leftwallwidth wallchar
    ,replicate leftpathwidth pathchar
    ,[if collision then '*' else playerchar]
    ,replicate rightpathwidth pathchar
    ,replicate rightwallwidth wallchar
    ,printf " %d " score'
    ]

  -- loop
  if collision
  then do
    putStrLn ""
    putStrLn "** BOOM! **"
    putStrLn $ "Bot's score was " ++ show score' ++ "." -- , try again!"
    -- threadDelay 5000000
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
