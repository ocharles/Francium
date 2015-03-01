{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Francium.Component
import Francium.Components.Form.Input
import Francium
import VirtualDom
import Data.Char
import GHCJS.Foreign

main :: IO ()
main =
  react (mdo input <- construct (Input value)
             let value =
                   stepper ""
                           (fmap (toJSString . map toUpper . fromJSString)
                                 (inputChanged (outputs input)))
             return (render input))
