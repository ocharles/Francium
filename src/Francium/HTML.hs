{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Francium.HTML
       (modifyElement, modifyElementB, attributes, namespace, text,
        emptyElement, module VirtualDom.HTML,
        module VirtualDom.HTML.Attributes, style)
       where

import Control.Arrow
import Control.Lens hiding (aside, children, coerce, pre)
import Control.Monad
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.State.Strict
import Data.Text.Lazy (toStrict)
import GHCJS.Foreign
import GHCJS.Types
import Prelude hiding (div, head, map, mapM, sequence, span)
import VirtualDom
import VirtualDom.HTML
import VirtualDom.HTML.Attributes hiding (abbr_, cite_, data_, form_, label_, option_, span_, style_, title_)
import VirtualDom.Prim hiding (text)
import qualified Clay
import qualified Data.Text as T
import qualified VirtualDom.Prim as Prim

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

modifyElement :: Functor m => State HTMLElement () -> HTML m a -> HTML m a
modifyElement s (HTML (WriterT m)) = HTML (WriterT (fmap (second (fmap (over _HTMLElement (execState s)))) m))

modifyElementB :: Applicative m => m (State HTMLElement ()) -> HTML m a -> HTML m a
modifyElementB s html =
  embed (modifyElement <$> s <*> observeHTML html)

text :: Applicative m => JSString -> HTML m ()
text t = HTML (WriterT (pure ((), pure (Prim.text t))))
