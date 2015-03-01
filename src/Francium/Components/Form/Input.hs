{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Francium.Components.Form.Input where

import Data.Monoid
import Francium.Component
import VirtualDom
import Reactive.Banana
import Reactive.Banana.Frameworks
import GHCJS.Types
import Francium.Hooks
import GHCJS.DOM.HTMLInputElement
import Unsafe.Coerce
  
data Input t = Input { inputValue :: Behavior t JSString }

instance Component Input where
  data Output behavior event Input = InputOutput{inputChanged ::
                                               event JSString}
  construct Input{..} =
    do (inputHook,onInput) <- newInputHook
       (mountHook,onMount) <- newMountHook
       reactimate
         ((\val el ->
             ghcjs_dom_html_input_element_set_value el val) <$>
          inputValue <@>
          (fmap unsafeCoerce onMount))
       return Instantiation {outputs =
                               InputOutput {inputChanged = onInput}
                            ,render =
                               (applyHooks (inputHook <> mountHook)
                                           input_) <$
                               stepper () (() <$ onInput)}
