{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Francium.Hooks where

import Control.Lens
import Control.Monad.State (MonadState)
import Control.Monad.Trans.State.Strict
import GHCJS.DOM.Event (eventGetTarget)
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.Types (GObject, toGObject, unsafeCastGObject)
import GHCJS.DOM.UIEvent
import Data.Foldable
import Control.Monad
import GHCJS.Marshal
import GHCJS.Types
import Reactive.Banana
import Reactive.Banana.Frameworks
import VirtualDom.Prim

data Hook =
  Hook {applyHook :: forall m. MonadState HTMLElement m => m ()}

instance Monoid Hook where
  mempty = Hook (return ())
  mappend (Hook a) (Hook y) = Hook (a >> y)

applyHooks :: Hook -> HTML -> HTML
applyHooks hook = _HTMLElement %~ (execState (applyHook hook))

newMouseOverHook :: Frameworks t => Moment t (Hook, Event t ())
newMouseOverHook =
  fmap (\(ev,handler) ->
          (Hook (on "mouseover" (const (handler ()))),ev))
       newEvent

newMouseOutHook :: Frameworks t => Moment t (Hook, Event t ())
newMouseOutHook =
  fmap (\(ev,handler) ->
          (Hook (on "mouseout" (const (handler ()))),ev))
       newEvent

newBlurHook :: Frameworks t => Moment t (Hook, Event t ())
newBlurHook =
  fmap (\(ev,handler) ->
          (Hook (on "blur" (const (handler ()))),ev))
       newEvent

newClickHook :: Frameworks t => Moment t (Hook, Event t ())
newClickHook =
  fmap (\(ev,handler) ->
          (Hook (on "click" (const (handler ()))),ev))
       newEvent

newInputHook :: Frameworks t => Moment t (Hook, Event t JSString)
newInputHook =
  fmap (\(ev,handler) ->
          (Hook (on "input"
                    (\ev ->
                       do t <- fromJSRef ev
                          for_ t
                               (\t' ->
                                  do t'' <- eventGetTarget t'
                                     for_ t''
                                          (htmlInputElementGetValue .
                                           castToHTMLInputElement >=> handler))))
          ,ev))
       newEvent

newKeyPressHook :: Frameworks t => Moment t (Hook, Event t Int)
newKeyPressHook =
  fmap (\(ev,handler) ->
          (Hook (on "keypress"
                    (\e ->
                       do t <- fromJSRef e
                          for_ t
                               (uiEventGetKeyCode .
                                -- A little messy, but we're working with a dom-delegator 'KeyEvent' here.
                                (unsafeCastGObject :: GObject -> UIEvent) .
                                toGObject >=>
                                handler)))
          ,ev))
       newEvent
