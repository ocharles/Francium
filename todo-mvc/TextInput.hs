{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module TextInput (TextInput(..), TextInput.value) where

import Francium
import Francium.Component
import Francium.Components.Form.Input
import GHCJS.Types

data TextInput t =
  TextInput {initialText :: JSString
            ,updateText :: Event t (JSString -> JSString)}

instance Component TextInput where
  data Output b e TextInput = TextInputOutput{value :: b JSString}
  construct ti =
    mdo input <-
          construct Input {inputValue = value}
        let value =
              accumB (initialText ti)
                     ((const <$>
                       inputChanged (outputs input)) `union`
                      (updateText ti))
        return Instantiation {render = render input
                             ,outputs =
                                TextInputOutput value}
