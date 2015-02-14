{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module NewItemAdder (NewItemAdder(..), addItem) where

import Control.Lens ((?=), at)
import Control.Monad.Trans.State.Strict (execState)
import Francium
import Francium.Component
import Francium.HTML (attrs)
import GHCJS.Types
import KeyPressObserver
import Prelude hiding (div, span)
import Reactive.Banana
import TextInput

-- | The 'NewItemAdder' component allows users to add new items to their to-do
-- list. Visually, it appears as an <input> box, and fires the 'addItem' event
-- when the user presses the return key on their keyboard.
data NewItemAdder = NewItemAdder

instance Component NewItemAdder where
  data Output behavior event NewItemAdder = NewItemOutput{addItem ::
                                                        event JSString}
  construct NewItemAdder =
    mdo -- Pressing return should clear the input field, allowing the user to
        -- add another to-do item.
        clearOnReturn <- trimE (const "" <$ complete)
        -- Construct an input field component, and transform this component
        -- with the 'KeyPressObserver' component transformer.
        inputComponent <-
          construct (KeyPressObserver
                       (TextInput {initialText = ""
                                  ,updateText = clearOnReturn}))
        let itemValue =
              TextInput.value (passThrough (outputs inputComponent))
            returnKeyCode = 13
            complete =
              filterE (== returnKeyCode) (keyPressed (outputs inputComponent))
        return Instantiation {render =
                                fmap (execState inputAttributes)
                                     (render inputComponent)
                             ,outputs =
                                NewItemOutput {addItem = itemValue <@ complete}}
    where inputAttributes =
            do attrs .
                 at "style" ?=
                 "-webkit-font-smoothing: antialiased; box-sizing: border-box; box-shadow: rgba(0, 0, 0, 0.027451) 0px -2px 1px inset; border: 1px none rgb(153, 153, 153); padding: 16px 16px 16px 60px; outline-style: none; line-height: 1.4em; font-size: 24px; width: 100%; margin: 0px; position: relative; background-color: rgba(0, 0, 0, 0);"
               attrs .
                 at "placeholder" ?=
                 "What needs to be done?"
               attrs .
                 at "autofocus" ?=
                 ""
