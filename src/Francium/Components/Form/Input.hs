{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Francium.Components.Form.Input where

import Control.FRPNow hiding (when)
import Control.Lens ((.=))
import Control.Monad (when)
import Data.Monoid
import Data.Foldable (for_)
import Francium.Component
import Francium.HTML hiding (for_)
import Francium.Hooks
import GHCJS.DOM.HTMLInputElement
import GHCJS.Types

data Input =
  Input {inputValue :: Behavior JSString
        ,inputDisabled :: Behavior Bool}

instance Component Input where
  data Output Input = InputOutput{inputChanged :: EvStream JSString}
  construct Input{..} =
    do (inputHook,onInput) <- newInputHook
       (renderHook,onRender) <- newRenderHook
       callStream
         (\els ->
            (do v <- sample inputValue
                sync (for_ els
                           (\el ->
                              let htmlInput = castToHTMLInputElement el
                              in do now <- htmlInputElementGetValue htmlInput
                                    when (not (eqRef (now `asTypeOf` v) v))
                                         (htmlInputElementSetValue htmlInput v)))))
         onRender
       return Instantiation {outputs =
                               InputOutput {inputChanged = onInput}
                            ,render =
                               -- We force the <input> element to be rerendered
                               -- whenever the 'input' event fires. This means
                               -- that even if the desired value doesn't change
                               -- we will reset the input field (essentially
                               -- locking it).
                               embed
                                 ((\isDisabled ->
                                     input_ (do disabled .= isDisabled
                                                applyHooks
                                                  (inputHook <> renderHook))
                                            mempty) <$>
                                  inputDisabled <*
                                  inputValue)}
