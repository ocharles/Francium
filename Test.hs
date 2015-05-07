{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Francium.Component
import Francium.Components.Form.Input
import Francium
import Francium.Hooks
import VirtualDom
import Data.Char
import GHCJS.Foreign

main :: IO ()
main =
  react (mdo input <- construct (Input v (pure False))
             (clickHook,onClick) <- newClickHook
             let v =
                   fmap (toJSString . show)
                        (accumB 0 (fmap (const (+ 1)) onClick))
             let value =
                   stepper ""
                           (fmap (toJSString . map toUpper . fromJSString)
                                 (inputChanged (outputs input)))
             return (div_ (mconcat [render input
                                   ,button_ (applyHooks clickHook) "Inc"])))
