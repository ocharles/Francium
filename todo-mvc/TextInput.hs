{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module TextInput (TextInput(..), TextInput.value) where

import Francium.Component
import Francium.Components.Form.Input
import GHCJS.Types
import Control.FRPNow

data TextInput =
  TextInput {initialText :: JSString
            ,updateText :: EvStream (JSString -> JSString)}

instance Component TextInput where
  data Output TextInput = TextInputOutput{value :: Behavior JSString}
  construct ti =
    mdo input <-
          construct Input {inputValue = value
                          ,inputDisabled =
                             pure False}
        value <-
          sample (foldEs (\a f -> f a)
                         (initialText ti)
                         (merge (const <$> inputChanged (outputs input))
                                (updateText ti)))
        return Instantiation {render = render input
                             ,outputs =
                                TextInputOutput value}
