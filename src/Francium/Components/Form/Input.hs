{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Francium.Components.Form.Input where

import Control.FRPNow hiding (when)
import Control.Lens
import Control.Monad (when, void)
import Control.Monad.Trans.Class
import Data.Foldable (for_)
import Data.Monoid
import Francium.HTML hiding (for_)
import Francium.Hooks
import GHCJS.DOM.HTMLInputElement
import GHCJS.Types

data TextInputConfig =
  TextInputConfig {textInputValue :: Behavior JSString
                  ,textInputEnabled :: Behavior Bool}

defaultTextInputConfig :: TextInputConfig
defaultTextInputConfig =
  TextInputConfig {textInputValue = pure ""
                  ,textInputEnabled = pure True}

data TextInput =
  TextInput {renderTextInput :: HTML Behavior ()
            ,textInputChanges :: EvStream JSString}

newTextInput :: TextInputConfig -> Now TextInput
newTextInput TextInputConfig{..} =
  do (inputHook,onInput) <- newInputHook
     (renderHook,onRender) <- newRenderHook
     callStream
       (\els ->
          (do v <- sample textInputValue
              sync (for_ els
                         (\el ->
                            do let htmlInput = castToHTMLInputElement el
                               now <- htmlInputElementGetValue htmlInput
                               when (not (eqRef (now `asTypeOf` v) v))
                                    (htmlInputElementSetValue htmlInput v)))))
       onRender
     return TextInput {textInputChanges = onInput
                      ,renderTextInput =
                         -- We force the <input> element to be rerendered
                         -- whenever the 'input' event fires. This means
                         -- that even if the desired value doesn't change
                         -- we will reset the input field (essentially
                         -- locking it).
                         input_
                           (do disabled <~ lift (fmap not textInputEnabled)
                               lift (do void textInputValue
                                        void (fromChanges ()
                                                          (void onInput)))
                               applyHooks (inputHook <> renderHook))
                           mempty}
