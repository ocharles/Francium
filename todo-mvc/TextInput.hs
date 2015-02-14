{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module TextInput (TextInput(..), TextInput.value) where

import Control.Lens ((?=), at)
import Francium
import Francium.HTML as HTML
import Francium.Component
import GHCJS.Types

data TextInput =
  TextInput {initialText :: JSString
            ,updateText :: AnyMoment Event (JSString -> JSString)}

instance Component TextInput where
  data Output b e TextInput = TextInputOutput{value :: b JSString}
  construct ti =
    do inputEv <- newDOMEvent
       updateText' <- now (updateText ti)
       let itemValue =
             accumB (initialText ti)
                    ((const <$> domEvent inputEv) `union`
                     updateText')
       return Instantiation {render =
                               fmap (\v ->
                                       with input
                                            (do onInput inputEv
                                                HTML.value ?= v)
                                            [])
                                    itemValue
                            ,outputs =
                               TextInputOutput itemValue}
