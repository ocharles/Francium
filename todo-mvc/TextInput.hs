{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module TextInput where

import Francium.HTML
import qualified Francium.Components.Form.Input as TI
import GHCJS.Types
import Control.FRPNow

data TextInputConfig =
  TextInputConfig {textInputInitialText :: JSString
                  ,textInputUpdates :: EvStream (JSString -> JSString)}

defaultTextInputConfig :: TextInputConfig
defaultTextInputConfig =
  TextInputConfig {textInputInitialText = ""
                  ,textInputUpdates = mempty}

data TextInput =
  TextInput {textInputValue :: Behavior JSString
            ,renderTextInput :: HTML Behavior ()}

newTextInput :: TextInputConfig -> Now TextInput
newTextInput TextInputConfig{..} =
  mdo input <-
        TI.newTextInput TI.defaultTextInputConfig {TI.textInputValue = value}
      value <-
        sample (foldEs (\a f -> f a)
                       textInputInitialText
                       (merge (const <$> TI.textInputChanges input) textInputUpdates))
      return TextInput {renderTextInput = TI.renderTextInput input
                       ,textInputValue = value}
