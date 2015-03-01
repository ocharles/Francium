{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module TextInput (TextInput(..), TextInput.value) where

import Control.Lens ((?=), at)
import Francium
import Francium.Component
import Francium.Hooks
import GHCJS.Types
import VirtualDom.HTML
import VirtualDom.Prim

data TextInput t =
  TextInput {initialText :: JSString
            ,updateText :: Event t (JSString -> JSString)}

instance Component TextInput where
  data Output b e TextInput = TextInputOutput{value :: b JSString}
  construct ti =
    do (inputHook,inputEv) <- newInputHook
       let itemValue =
             accumB (initialText ti)
                    ((const <$> inputEv) `union`
                     (updateText ti))
       return Instantiation {render =
                               fmap (\v ->
                                       with (applyHooks inputHook input_)
                                            (properties .
                                             at "value" ?=
                                             v)
                                            [])
                                    itemValue
                            ,outputs =
                               TextInputOutput itemValue}
