{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Francium
  ( -- * Running Francium applications
    react

    -- * Building HTML trees
  , HTML, with, into
  , DOMEvent
  , newDOMEvent
  , domEvent

    -- * 'reactive-banana' re-exports
    -- ** Core Combinators
  , Event
  , Behavior
  , union
  , stepper
  , (<@>)
  , (<@)
  , accumB
  , accumE
  , initial
  , never
  , whenE

    -- ** Network Modifications
  , Moment
  , Frameworks
  , AddHandler(..)
  , fromAddHandler
  , reactimate
  , changes
  , reactimate'
  , FrameworksMoment(..)
  , execute
  , liftIO
  , liftIOLater
  , newEvent
  , Handler

    -- ** Switching Combinators
  , AnyMoment
  , anyMoment
  , now
  , Trim(..)
  , switchB
  , switchE

    -- * Bidirectional Components
  , Tidings
  , tidings
  , rumors
  , facts

    -- * Re-exported modules
  , module Control.Applicative
  , lmap, dimap
  ) where

import Prelude hiding (div, mapM, sequence)

import Data.Profunctor
import Francium.DOMEvent
import Francium.HTML (HTML, div, newTopLevelContainer, renderTo, with, into)
import Francium.Tidings
import Control.Applicative
import Control.Monad.IO.Class
import GHCJS.Types
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified Francium.HTML as HTML
import Control.Lens (at, (?=))

--------------------------------------------------------------------------------
class Trim a where
  type Trimmed a :: *
  type Time a :: *
  trim :: a -> Moment (Time a) (Trimmed a)

instance Trim (Behavior t a) where
  type Trimmed (Behavior t a) = AnyMoment Behavior a
  type Time (Behavior t a) = t
  trim = trimB

instance Trim (Event t a) where
  type Trimmed (Event t a) = AnyMoment Event a
  type Time (Event t a) = t
  trim = trimE

--------------------------------------------------------------------------------
react :: (forall t. Frameworks t => Moment t (Behavior t HTML)) -> IO ()
react app = do
  container <- newTopLevelContainer

  _ <- initDomDelegator

  eventNetwork <- compile $ do
    document <- fmap (with div (HTML.attrs . at "style" ?= "min-height: 100%; height: 100%;") . pure) <$> app
    initial document >>= liftIO . renderTo container
    documentChanged <- changes document
    reactimate' $ fmap (renderTo container) <$> documentChanged

  actuate eventNetwork

--------------------------------------------------------------------------------
data DOMDelegator

#ifndef HLINT

foreign import javascript unsafe
  "console.log('Initializing dom-delegator'); var dd = DOMDelegator(); dd.listenTo('mouseover'); dd.listenTo('mouseout'); $r = dd;"
  initDomDelegator :: IO (JSRef DOMDelegator)

#endif
