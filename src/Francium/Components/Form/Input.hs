{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Francium.Components.Form.Input where

import Control.Monad (void, when)
import Data.Monoid
import Francium.Component
import Francium.HTML
import Francium.Hooks
import GHCJS.DOM.HTMLInputElement
import GHCJS.Types
import Reactive.Banana
import Reactive.Banana.Frameworks

data Input t = Input { inputValue :: Behavior t JSString }

instance Component Input where
  data Output behavior event Input = InputOutput{inputChanged ::
                                               event JSString}
  construct Input{..} =
    do (inputHook,onInput) <- newInputHook
       (renderHook,onRender) <- newRenderHook
       reactimate
         (fmap (\(v,el) ->
                  let htmlInput = castToHTMLInputElement el
                  in do now <- htmlInputElementGetValue htmlInput
                        when (now /= v)
                             (htmlInputElementSetValue htmlInput v))
               ((,) <$> inputValue <@> onRender))
       return Instantiation {outputs =
                               InputOutput {inputChanged = onInput}
                            ,render =
                               -- We force the <input> element to be rerendered
                               -- whenever the 'input' event fires. This means
                               -- that even if the desired value doesn't change
                               -- we will reset the input field (essentially
                               -- locking it).
                               embed
                                 (input_ (applyHooks (inputHook <> renderHook)) mempty <$
                                  inputValue <*
                                  (stepper () (void onInput)))}
