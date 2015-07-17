{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Francium.Component
import Francium.Components.Form.Input
import Control.FRPNow
import Data.Char
import Francium
import Francium.HTML
import Francium.Hooks
import GHCJS.Foreign
import VirtualDom

main :: IO ()
main =
  react (do (clickHook,onClick) <- newClickHook
            v <-
              fmap (fmap (toJSString . show))
                   (sample (foldEs (\x _ -> x + 1) 0 onClick))
            input <-
              construct (Input v (pure False))
            value <-
              sample (fromChanges
                        ""
                        (fmap (toJSString . map toUpper . fromJSString)
                              (inputChanged (outputs input))))
            return (div_ (mconcat [render input
                                  ,button_ (applyHooks clickHook) "Inc"])))
