{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Francium.Hooks where

import Control.Arrow (second)
import Control.FRPNow
import Control.Lens
import Control.Monad
import Control.Monad.State (MonadState)
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.Function (fix)
import Data.Monoid ((<>))
import GHCJS.DOM.Element (castToElement)
import GHCJS.DOM.Event (eventGetTarget)
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.Types (Element, toGObject, unsafeCastGObject)
import GHCJS.DOM.UIEvent
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import qualified GHCJS.DOM.Event as DOM
import qualified VirtualDom.Prim as VDom

data Hook =
  Hook {applyHook :: forall m. MonadState VDom.HTMLElement m => m ()}

instance Monoid Hook where
  mempty = Hook (return ())
  mappend (Hook a) (Hook y) = Hook (a >> y)

applyHooks :: Hook -> State VDom.HTMLElement ()
applyHooks hook = applyHook hook

on :: JSString -> Now (Hook, EvStream DOM.Event)
on eventName =
  fmap (\(ev,handler) ->
          (Hook (VDom.on eventName
                         (\e ->
                            do t <- fromJSRef e
                               for_ t handler))
          ,ev))
       callbackStream

newMouseOverHook :: Now (Hook, EvStream ())
newMouseOverHook = fmap (second void) (on "mouseover")

newMouseOutHook :: Now (Hook, EvStream ())
newMouseOutHook = fmap (second void) (on "mouseout")

newBlurHook :: Now (Hook, EvStream ())
newBlurHook = fmap (second void) (on "blur")

newClickHook :: Now (Hook, EvStream ())
newClickHook = fmap (second void) (on "click")

newInputHook :: Now (Hook, EvStream JSString)
newInputHook =
  do (hook,evs) <- on "input"
     (evs',handler) <- callbackStream
     callIOStream
       (\t' ->
          do t'' <- eventGetTarget t'
             for_ t''
                  (\target ->
                     htmlInputElementGetValue (castToHTMLInputElement target) >>=
                     handler))
       evs
     return (hook,evs')

newKeyPressHook :: Now (Hook, EvStream Int)
newKeyPressHook =
  do (hook,evs) <- on "keypress"
     (evs',handler) <- callbackStream
     -- A little messy, but we're working with a dom-delegator 'KeyEvent' here.
     callIOStream
       (\t' ->
          uiEventGetKeyCode (unsafeCastGObject (toGObject t') :: UIEvent) >>=
          handler)
       evs
     return (hook,evs')

-- | The render hook emits an event whenever the 'HTML' it is applied to is
-- rendered to the DOM.
newRenderHook :: Now (Hook,EvStream Element)
newRenderHook =
  fmap (\(ev,handler) ->
          (Hook     -- We need to make sure we don't clobber any hooks that have
                    -- already been added.

             (do name <-
                   fix (\retry (x:xs) ->
                          do m <-
                               use (VDom.properties .
                                    at (toJSString ("hook-" <> x)))
                             case m of
                               Nothing ->
                                 return (toJSString x)
                               Just _ -> retry xs)
                       ["render" ++ suffix | suffix <-
                                              iterate ('_' :) ""]
                 VDom.registerHook
                   name
                   (\el _ ->
                      fromJSRef el >>=
                      maybe (putStrLn "Render hook: Cast to Element failed. Please report this as a bug!")
                            (handler . castToElement)))
          ,ev))
       callbackStream

newHoverHook :: Now (Hook, Behavior Bool)
newHoverHook =
  do (mouseOverHook,mouseOver) <- newMouseOverHook
     (mouseOutHook,mouseOut) <- newMouseOutHook
     mouseHovering <-
       sample (fromChanges
                 False
                 (merge (True <$ mouseOver)
                        (False <$ mouseOut)))
     let hook = mouseOverHook <> mouseOutHook
     return (hook,mouseHovering)
