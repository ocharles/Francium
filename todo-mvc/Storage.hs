{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Storage where

import Data.Aeson (ToJSON(..), FromJSON(..), encode)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics
import GHCJS.Foreign 
import GHCJS.Types

data ToDoItem = ToDoItem { title :: String, complete :: Bool }
  deriving (Generic)

instance ToJSON ToDoItem
instance FromJSON ToDoItem

store :: [ToDoItem] -> IO ()
store = localStorageSetItem "francium-todos" . toJSString . decodeUtf8 . toStrict . encode

foreign import javascript unsafe
  "localStorage.setItem($1, $2)"
  localStorageSetItem :: JSString -> JSString -> IO ()
