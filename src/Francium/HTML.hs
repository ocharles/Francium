{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Francium.HTML (style) where

import Control.Applicative
import Control.Lens hiding (aside, children, coerce, pre)
import Control.Monad
import Control.Monad.State
import Data.Coerce (coerce)
import Data.IORef
import Data.List (intersperse, intercalate)
import Data.Maybe
import Data.String (IsString(..))
import Data.Text.Lazy (toStrict)
import Francium.DOMEvent
import GHCJS.DOM
import GHCJS.DOM.Document
import GHCJS.DOM.Element
import GHCJS.DOM.Node
import GHCJS.Foreign
import GHCJS.Types
import Prelude hiding (div, head, map, mapM, sequence, span)
import System.IO.Unsafe
import VirtualDom.Prim
import qualified Clay
import qualified Data.Immutable as Immutable
import qualified Data.Text as T
  
style :: Setter' HTMLElement Clay.Css
style =
  attributes .
  at "style" .
  anon "" (isEmptyStr . fromJSString) .
  sets (\f x ->
          toJSString
            (T.init (T.tail (toStrict (Clay.renderWith Clay.compact
                                                       []
                                                       (f (return ())))))))
  where isEmptyStr = (== ("" :: String))
