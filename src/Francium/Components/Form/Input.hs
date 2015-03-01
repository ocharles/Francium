{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Francium.Components.Form.Input where

import Control.Monad (void)
import Data.Monoid
import Francium.Component
import Francium.Hooks
import GHCJS.DOM.HTMLInputElement
import GHCJS.Foreign
import GHCJS.Types
import Reactive.Banana
import Reactive.Banana.Frameworks
import Unsafe.Coerce
import VirtualDom
  
data Input t = Input { inputValue :: Behavior t JSString }

instance Component Input where
  data Output behavior event Input = InputOutput{inputChanged ::
                                               event JSString}
  construct Input{..} =
    do (inputHook,onInput) <- newInputHook
       (renderHook,onRender) <- newRenderHook
       reactimate
         (fmap (\(v,el) ->
                  ghcjs_dom_html_input_element_set_value (unsafeCoerce el)
                                                         v)
               ((,) <$> inputValue <@> onRender))
       return Instantiation {outputs =
                               InputOutput {inputChanged = onInput}
                            ,render =
                               -- We force the <input> element to be rerendered
                               -- whenever the 'input' event fires. This means
                               -- that even if the desired value doesn't change
                               -- we will reset the input field (essentially
                               -- locking it).
                               applyHooks (inputHook <> renderHook)
                                          input_ <$
                               inputValue <*
                               (stepper () (void onInput))}
