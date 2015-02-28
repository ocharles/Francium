{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Francium.HTML
  ( attributes
  , classes
  , style
  )
where

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
  
attributes :: Traversal' HTMLElement Immutable.Map
attributes f (HTMLElement vNode) =
  fmap (HTMLElement . setVNodeAttributes vNode)
       (f (getVNodeAttributes vNode))
  
classes :: Traversal' HTMLElement [String]
classes =
  attributes .
  at "class" .
  anon "" (isEmptyStr . fromJSString) .
  iso (words . fromJSString)
      (toJSString . unwords)
  where isEmptyStr = (== ("" :: String))

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

value :: Traversal' HTMLElement (Maybe JSString)
value = properties . at "value"

foreign import javascript safe
  "Immutable.Map($1.properties.attributes)"
  getVNodeAttributes :: JSRef VNode -> Immutable.Map

foreign import javascript safe
  "new VNode($1.tagName, Immutable.Map($1.properties).set('attributes', $2).toJS(), $1.children, $1.key, $1.namespace)"
  setVNodeAttributes :: JSRef VNode -> Immutable.Map -> JSRef VNode
