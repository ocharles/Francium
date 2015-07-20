{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module NewItemAdder
       (NewItemAdder, newNewItemAdder, newItemAdderAdditions,
        renderNewItemAdder)
       where

import Control.Monad
import Control.FRPNow
import Control.Lens ((?=), (.=))
import Francium.CSS
import Francium.HTML
import Francium.Hooks
import GHCJS.Foreign
import GHCJS.Types
import Prelude hiding (div, span)
import TextInput

-- | The 'NewItemAdder' component allows users to add new items to their to-do
-- list. Visually, it appears as an <input> box, and fires the 'addItem' event
-- when the user presses the return key on their keyboard.

data NewItemAdder =
  NewItemAdder {_newItemAdderAdditions :: EvStream JSString
               ,_renderNewItemAdder :: HTML Behavior ()}

newItemAdderAdditions :: NewItemAdder -> EvStream JSString
newItemAdderAdditions = _newItemAdderAdditions

renderNewItemAdder :: NewItemAdder -> HTML Behavior ()
renderNewItemAdder = _renderNewItemAdder

newNewItemAdder :: Now NewItemAdder
newNewItemAdder =
  mdo
      -- Construct an input field component.
      inputComponent <- newTextInput defaultTextInputConfig -- fmap (const (const "")) returnPressed})
      -- We add a new "hook" to the event network to observe whenever the user
      -- presses a key. Later, we will filter this event stream to only fire
      -- when return is pressed.
      (hookKeyPress,keyPressed) <- newKeyPressHook
      let
          -- The keyPressed event gives us an event whenever a key is pressed.
          -- We only need to know when the user presses return, so we filter
          -- the event stream accordingly.
          returnPressed = listenForReturn keyPressed
          -- The itemValue is the title of the to-do item being added. We
          -- pass through the behavior from the TextInput component, which
          -- provides us with the contents of the text box.
          itemValue = textInputValue inputComponent
      finalValue <-
        sample (prev ("" :: String)
                     (fmap fromJSString itemValue))
      return NewItemAdder {_renderNewItemAdder =
                             -- To render the component, we simply reskin the
                             -- TextInput component
                             embed
                               (fmap (modifyElement
                                        (do applyHooks hookKeyPress
                                            inputAttributes))
                                     (observeHTML (renderTextInput inputComponent)))
                          ,_newItemAdderAdditions =
                             snapshots (fmap toJSString finalValue)
                                       (void returnPressed)}
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
             placeholder_ ?= "What needs to be done?"
             autofocus_ ?= ""

listenForReturn :: (Num a, Eq a) => EvStream a -> EvStream a
listenForReturn = filterEs (== returnKeyCode)
  where returnKeyCode = 13
