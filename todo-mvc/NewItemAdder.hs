{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module NewItemAdder (NewItemAdder(..), addItem) where

import Clay as CSS hiding (render, style)
import Control.Lens ((?=), (.=), at)
import Control.Monad.Trans.State.Strict (execState)
import Francium
import Francium.Component
import Francium.HTML (attrs, style)
import GHCJS.Types
import KeyPressObserver
import Prelude hiding (div, span)
import Reactive.Banana
import TextInput
import HoverObserver

-- | The 'NewItemAdder' component allows users to add new items to their to-do
-- list. Visually, it appears as an <input> box, and fires the 'addItem' event
-- when the user presses the return key on their keyboard.
data NewItemAdder t =
  NewItemAdder

instance Component NewItemAdder where
  data Output behavior event NewItemAdder = NewItemOutput{addItem ::
                                                        event JSString}
  construct NewItemAdder =
    mdo
        -- Construct an input field component.
        inputComponent <-
          construct (TextInput {initialText = ""
                               ,updateText =
                                  fmap (const (const "")) returnPressed})
        (hookKeyPress,keyPressed) <- newKeyPressObserver
        let
            -- The 'KeyPressObserver' gives us an event whenever a key is pressed.
            -- We only need to know when the user presses return, so we filter
            -- the event stream accordingly.
            returnPressed = listenForReturn keyPressed
            -- The itemValue is the title of the to-do item being added. We
            -- pass through the behavior from the TextInput component, which
            -- provides us with the contents of the text box.
            itemValue =
              TextInput.value (outputs inputComponent)
        return Instantiation {render =
                                -- To render the component, we simply reskin the
                                -- TextInput component
                                fmap
                                  (applyHooks hookKeyPress .
                                   execState inputAttributes)
                                  (render inputComponent)
                             ,outputs =
                                -- The outputs of this component is an Event
                                -- that samples the contents of the input field
                                -- whenever return is pressed.
                                NewItemOutput {addItem = itemValue <@
                                                         returnPressed}}
    where inputAttributes =
            do style .=
                 (do boxSizing borderBox
                     insetBoxShadow inset
                                    (px 0)
                                    (px (-2))
                                    (px 1)
                                    (rgba 0 0 0 7)
                     borderStyle none
                     padding (px 15)
                             (px 15)
                             (px 15)
                             (px 60)
                     outlineStyle none
                     lineHeight (em 1.5)
                     fontSize (px 24)
                     width (pct 100)
                     sym margin (px 0)
                     position relative
                     backgroundColor (rgba 0 0 0 0))
               attrs .
                 at "placeholder" ?=
                 "What needs to be done?"
               attrs .
                 at "autofocus" ?=
                 ""

listenForReturn :: (Num a, Eq a) => Event t a -> Event t a
listenForReturn = filterE (== returnKeyCode)
  where returnKeyCode = 13
