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

import Control.Applicative
import Control.Lens (at, (?=))
import Control.Monad.IO.Class
import Data.Profunctor
import Francium.DOMEvent
import Francium.Tidings
import GHCJS.Types
import Prelude hiding (div, mapM, sequence)
import Reactive.Banana
import Reactive.Banana.Frameworks
import VirtualDom
import VirtualDom.Prim
import qualified Francium.HTML as HTML

--------------------------------------------------------------------------------
react :: (forall t. Frameworks t => Moment t (Behavior t HTML)) -> IO ()
react app = do
  container <- bodyContainer

  _ <- initDomDelegator

  eventNetwork <- compile $ do
    document <- app
    initial document >>= liftIO . renderTo container
    documentChanged <- changes document
    reactimate' $ fmap (renderTo container) <$> documentChanged

  actuate eventNetwork
