{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Francium.HTML
       (HTML, text, with, into, modifyElement, attributes, namespace, emptyElement, module VirtualDom.HTML,
        module VirtualDom.HTML.Attributes, style)
       where

import Control.Lens hiding (aside, children, coerce, pre)
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Text.Lazy (toStrict)
import GHCJS.Foreign
import Prelude hiding (div, head, map, mapM, sequence, span)
import VirtualDom
import VirtualDom.HTML
import VirtualDom.HTML.Attributes hiding (abbr_, cite_, data_, form_, label_, option_, span_, style_, title_)
import VirtualDom.Prim
import qualified Clay
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

modifyElement :: State HTMLElement () -> HTML -> HTML
modifyElement m = over _HTMLElement (execState m)
