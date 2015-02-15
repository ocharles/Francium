{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Storage where

import Data.Aeson (ToJSON(..), FromJSON(..), encode, decodeStrict)
import Data.ByteString.Lazy (toStrict)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics
import GHCJS.Foreign 
import GHCJS.Types

data ToDoItem = ToDoItem { title :: String, complete :: Bool }
  deriving (Generic, Show)

instance ToJSON ToDoItem
instance FromJSON ToDoItem

store :: [ToDoItem] -> IO ()
store = localStorageSetItem localStorageKey . toJSString . decodeUtf8 . toStrict .
                                                                        encode

retrieve :: IO (Maybe [ToDoItem])        
retrieve =
  fmap (decodeStrict . encodeUtf8 . fromJSString)
       (localStorageGetItem localStorageKey)

localStorageKey :: JSString
localStorageKey = "francium-todos"

foreign import javascript unsafe
  "localStorage.setItem($1, $2)"
  localStorageSetItem :: JSString -> JSString -> IO ()

foreign import javascript unsafe
  "localStorage.getItem($1)"
  localStorageGetItem :: JSString -> IO JSString
