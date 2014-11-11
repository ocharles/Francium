{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
module Francium
  ( -- * Running Francium applications
    react

    -- * Building HTML trees
  , HTML, (</>)
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
  , trim
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
import Francium.HTML (HTML, (</>), div, newTopLevelContainer, renderTo)
import Francium.Tidings
import Control.Applicative
import Control.Monad.IO.Class
import GHCJS.Types
import Reactive.Banana
import Reactive.Banana.Frameworks

--------------------------------------------------------------------------------
class Trim f where trim :: f t a -> Moment t (AnyMoment f a)
instance Trim Behavior where trim = trimB
instance Trim Event where trim = trimE

--------------------------------------------------------------------------------
react :: (forall t. Frameworks t => Moment t (Behavior t HTML)) -> IO ()
react app = do
  container <- newTopLevelContainer

  _ <- initDomDelegator

  eventNetwork <- compile $ do
    document <- fmap ((div </>) . pure) <$> app
    initial document >>= liftIO . renderTo container
    documentChanged <- changes document
    reactimate' $ fmap (renderTo container) <$> documentChanged

  actuate eventNetwork

--------------------------------------------------------------------------------
data DOMDelegator

#ifndef HLINT

foreign import javascript unsafe
  "console.log('Initializing dom-delegator'); $r = DOMDelegator();"
  initDomDelegator :: IO (JSRef DOMDelegator)

#endif
