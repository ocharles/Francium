{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Francium.Hooks where

import Control.Lens
import Control.Monad
import Control.Monad.State (MonadState)
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.Function (fix)
import Data.Monoid ((<>))
import Debug.Trace
import GHCJS.DOM.Element (castToElement)
import GHCJS.DOM.Event (eventGetTarget)
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.Types (Element, GObject, toGObject, unsafeCastGObject)
import GHCJS.DOM.UIEvent
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import Reactive.Banana
import Reactive.Banana.Frameworks
import VirtualDom.Prim
import qualified GHCJS.DOM.HTMLElement as DOM

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

-- | The render hook emits an event whenever the 'HTML' it is applied to is
-- rendered to the DOM.
newRenderHook :: Frameworks t => Moment t (Hook, Event t Element)
newRenderHook =
  fmap (\(ev,handler) ->
          (Hook     -- We need to make sure we don't clobber any hooks that have
                    -- already been added.

             (do name <-
                   fix (\retry (x:xs) ->
                          do m <-
                               use (properties .
                                    at (toJSString (traceId ("hook-" <> x))))
                             case m of
                               Nothing ->
                                 return (toJSString x)
                               Just _ ->
                                 trace (x ++ " in use")
                                       (retry xs))
                       ["render" ++ suffix | suffix <-
                                              iterate ('_' :) ""]
                 registerHook
                   name
                   (\el _ ->
                      fromJSRef el >>=
                      maybe (putStrLn "Render hook: Cast to Element failed. Please report this as a bug!")
                            (handler . castToElement)))
          ,ev))
       newEvent

newHoverHook :: Frameworks t => Moment t (Hook, Behavior t Bool)
newHoverHook =
  do (mouseOverHook,mouseOver) <- newMouseOverHook
     (mouseOutHook,mouseOut) <- newMouseOutHook
     let mouseHovering =
           accumB False
                  ((const True <$
                    mouseOver) `union`
                   (const False <$
                    mouseOut))
         hook = mouseOverHook <> mouseOutHook
     return (hook,mouseHovering)
