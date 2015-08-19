{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Concurrent
import Control.Event.Handler
import Control.Monad
import Reactive.Banana.Internal.Combinators
import System.Mem

main :: IO ()
main =
  do (ah,tick) <- newAddHandler
     network <-
       compile (do render <- fromAddHandler ah
                   addReactimate
                     (applyE (mapB (\x _ ->
                                      return (print x))
                                   (stepperB 0 render))
                             render))
     actuate network
     performGC
     tick 1
     putStrLn "GC"
     putStrLn "Done"
     tick 2
     mapM_ tick [3 .. 10]
