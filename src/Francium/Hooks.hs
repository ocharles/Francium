{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Francium.Hooks where

import Control.Arrow (second)
import Control.Lens
import Control.Monad
import Control.Monad.State (MonadState)
import Control.Monad.Trans.State.Lazy
import Data.Foldable
import Data.Function (fix)
import Data.Traversable (for)
import Data.Monoid ((<>))
import GHCJS.DOM.Element (castToElement)
import GHCJS.DOM.Event (eventGetTarget)
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.Types (Element, toGObject, unsafeCastGObject)
import GHCJS.DOM.UIEvent
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types
import Reactive.Banana
import Reactive.Banana.Frameworks
import qualified VirtualDom.Prim as VDom
import qualified GHCJS.DOM.Event as DOM

data Hook =
  Hook {applyHook :: forall m. MonadState VDom.HTMLElement m => m ()}

instance Monoid Hook where
  mempty = Hook (return ())
  mappend (Hook a) (Hook y) = Hook (a >> y)

applyHooks :: Hook -> State VDom.HTMLElement ()
applyHooks hook = applyHook hook

on :: Frameworks t => JSString -> Moment t (Hook, Event t DOM.Event)
on eventName =
  fmap (\(ev,handler) ->
          (Hook (VDom.on eventName
                         (\e ->
                            do t <- fromJSRef e
                               for_ t handler))
          ,ev))
       newEvent

newMouseOverHook :: Frameworks t => Moment t (Hook, Event t ())
newMouseOverHook = fmap (second void) (on "mouseover")

newMouseOutHook :: Frameworks t => Moment t (Hook, Event t ())
newMouseOutHook = fmap (second void) (on "mouseout")

newBlurHook :: Frameworks t => Moment t (Hook, Event t ())
newBlurHook = fmap (second void) (on "blur")

newClickHook :: Frameworks t => Moment t (Hook, Event t ())
newClickHook = fmap (second void) (on "click")

newInputHook :: Frameworks t => Moment t (Hook, Event t JSString)
newInputHook =
  do (hook,ev) <- on "input"
     ev' <-
       execute (fmap (\t' ->
                        FrameworksMoment
                          (liftIO (do t'' <- eventGetTarget t'
                                      for t''
                                          (htmlInputElementGetValue .
                                           castToHTMLInputElement))))
                     ev)
     return (hook,filterJust ev')

newKeyPressHook :: Frameworks t => Moment t (Hook, Event t Int)
newKeyPressHook =
  do (hook,ev) <- on "keypress"
     ev' <-
       -- A little messy, but we're working with a dom-delegator 'KeyEvent' here.
       execute
         (fmap (\t' ->
                  FrameworksMoment (liftIO (uiEventGetKeyCode (unsafeCastGObject (toGObject t') :: UIEvent))))
               ev)
     return (hook,ev')

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
