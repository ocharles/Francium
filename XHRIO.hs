{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE InterruptibleFFI #-}
{-# LANGUAGE RecordWildCards #-}
module XHRIO where

import Control.Arrow
import Data.Maybe
import Data.List.Split
import Control.Applicative
import Data.Foldable (for_)
import GHCJS.Foreign
import GHCJS.Types

data Request = Request
  { rqWithCredentials :: Bool
  , rqURL :: String
  , rqMethod :: Method
  , rqHeaders :: [(String, String)]
  , rqPayload :: Maybe JSString
  }

data Method = GET | POST | DELETE | PUT deriving (Eq, Show)

instance ToJSString Method where
  toJSString = toJSString . show

data Response = Response { resStatus :: Int
                         , resHeaders :: [(String, String)]
                         }

request :: Request -> IO Response
request Request{..} = do
  xhr <- jsNewXHR
  jsXHROpen xhr (toJSString rqMethod) (toJSString rqURL)
  jsXHRSetWithCredentials xhr (toJSBool rqWithCredentials)
  for_ rqHeaders $ \(k, v) -> jsXHRSetRequestHeader xhr (toJSString k) (toJSString v)
  jsXHRSend xhr (fromMaybe jsNull rqPayload)
  Response
   <$> jsXHRGetStatus xhr
   <*> (map (second (drop 2) . break (== ':')) . splitOn "\r\n" . fromJSString
          <$> jsXHRGetAllResponseHeaders xhr)

--------------------------------------------------------------------------------
data XHR

foreign import javascript unsafe
 "$r = new XMLHttpRequest();\
 \$r.latestProgressMessage = null;\
 \$r.awaitingProgress = null;\
 \$r.incrementalPos = 0;\
 \$r.err = null"
 jsNewXHR :: IO (JSRef XHR)

foreign import javascript unsafe
 "$1.withCredentials = $2;"
  jsXHRSetWithCredentials :: JSRef XHR -> JSBool -> IO ()

foreign import javascript unsafe
 "$1.open($2, $3, true);"
  jsXHROpen :: JSRef XHR -> JSString -> JSString -> IO ()

foreign import javascript unsafe
 "$1.setRequestHeader($2, $3);"
  jsXHRSetRequestHeader :: JSRef XHR -> JSString -> JSString -> IO ()

foreign import javascript interruptible
 "$1.onload = function(e) { $c(); };\
 \$1.onerror = function(e) { $1['h$err'] = true; $c(); };\
 \$1.send($2);"
 jsXHRSend :: JSRef XHR -> JSRef a -> IO ()

foreign import javascript unsafe
 "$1.status"
 jsXHRGetStatus :: JSRef XHR -> IO Int

foreign import javascript unsafe
 "$1.getAllResponseHeaders()"
 jsXHRGetAllResponseHeaders :: JSRef XHR -> IO JSString
