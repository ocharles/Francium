{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Francium
  ( -- * Running Francium applications
    react
  , FranciumApp

    -- * Building HTML trees
  , HTML, with, into, text, modifyElement

    -- * Components
  , Component(..)
  , render
  , outputs

    -- * 'reactive-banana' re-exports
    -- ** Core Combinators
  , Event
  , Behavior
  , union
  , unions
  , stepper
  , (<@>)
  , (<@)
  , accumB
  , accumE
  , initial
  , never
  , whenE
  , filterE

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
  , trim

    -- *** Switching Components
  , trimComponent

    -- * Haskell browser functions
  , nextTick

    -- * Embedding 'IO'
  , ioAsEvent
  , ioAsEventLater

    -- * Re-exported modules
  , module Control.Applicative
  , lmap, dimap
  ) where

import Control.Applicative
import Control.Monad ((<=<))
import Control.Monad.IO.Class
import Data.IORef
import Data.Profunctor
import Francium.Component
import Francium.HTML
import GHCJS.Foreign
import GHCJS.Types
import Prelude hiding (div, mapM, sequence)
import Reactive.Banana
import Reactive.Banana.Frameworks
import VirtualDom

--------------------------------------------------------------------------------
type FranciumApp = forall t. Frameworks t => Moment t (Behavior t HTML)

react :: FranciumApp -> IO ()
react app =
  do container <- newTopLevelContainer
     _ <- initDomDelegator
     initialRender <- newIORef div_
     eventNetwork <-
       compile (do document <- app
                   do html <- initial document
                      liftIOLater (writeIORef initialRender html)
                   documentChanged <- changes document
                   reactimate' (fmap (fmap (renderTo container)) documentChanged))
     do html <- readIORef initialRender
        renderTo container html
     actuate eventNetwork

--------------------------------------------------------------------------------
foreign import javascript unsafe
  "window.nextTick($1)"
  ffiNextTick :: JSFun (IO ()) -> IO ()

nextTick :: IO () -> IO ()
nextTick = ffiNextTick <=< syncCallback AlwaysRetain True

--------------------------------------------------------------------------------
-- | Immediately begin performing an 'IO' action asynchronously, and fire the
-- 'Event' once when it completes.
ioAsEvent :: Frameworks t => IO a -> Moment t (Event t a, ThreadId)
ioAsEvent io =
  do (ioComplete,fireIoComplete) <- newEvent
     thread <- liftIO (forkIO (io >>= fireIoComplete))
     return (ioComplete, thread)

-- | Build a deferred computation that when 'execute'd will perform the given
-- 'IO' action and deliver its result in an 'Event'.
ioAsEventLater :: IO a -> FrameworksMoment (AnyMoment Event a)
ioAsEventLater io =
  FrameworksMoment (ioAsEvent io >>= trim . fst)
