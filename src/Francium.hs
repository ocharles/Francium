{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Francium where

import Control.Concurrent
import Debug.Trace
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Writer.Strict
       (WriterT(..), execWriterT, Writer, execWriter)
import Control.Monad.Writer (MonadWriter(..))
import Data.Char
import Data.Coerce
import Data.Foldable
import Data.JSString (pack, unpack)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.String
import Data.Unique
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.CSSStyleDeclaration
import GHCJS.DOM.CharacterData (setData)
import GHCJS.DOM.Document (createElement, createTextNode, getBody)
import GHCJS.DOM.Element (getStyle, setAttribute)
import GHCJS.DOM.EventTarget (addEventListener)
import GHCJS.DOM.EventTargetClosures
import GHCJS.DOM.HTMLButtonElement (castToHTMLButtonElement)
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.Node
import GHCJS.DOM.Text (Text)
import GHCJS.DOM.Types (IsGObject)
import GHCJS.DOM.Types (Node, castToNode)
import GHCJS.DOM.Types (UIEvent, castToElement)
import GHCJS.Types
import JavaScript.Web.AnimationFrame
import Reactive.Banana
import Reactive.Banana.Frameworks
import System.IO.Unsafe
import System.Mem

data FranciumEnv t =
  FranciumEnv {onRender :: Event t ()
              ,parentNode :: Node
              ,appClock :: Event t Double}

newtype Francium t a =
  Francium {runFrancium :: ReaderT (FranciumEnv t) (WriterT (Behavior t [MarkedNode]) (Moment t)) a}

instance Monad (Francium t) where
  return a = Francium (return a)
  Francium a >>= f =
    Francium (a >>= runFrancium . f)

instance Frameworks t => MonadIO (Francium t) where
  liftIO m = Francium (liftIO m)

instance Applicative (Francium t) where
  pure = return
  (<*>) = ap

instance Functor (Francium t) where
  fmap f (Francium m) = Francium (fmap f m)

instance MonadFix (Francium t) where
  mfix f = Francium (mfix (runFrancium . f))

rb :: Frameworks t
   => Moment t a -> Francium t a
rb m = Francium (lift (lift m))

instance Monoid a => Monoid (Behavior t a) where
  mempty = pure mempty
  mappend = liftA2 mappend

text_ :: Frameworks t
      => Behavior t JSString -> Francium t ()
text_ contents =
  do tNode <-
       do initialContent <- rb (initial contents)
          liftIO (unsafeInterleaveIO (newTextElement initialContent)) -- TODO Still necessary? Arose when trying to read if a button had or hadn't been clicked as the text of a button itself
     env <- Francium ask
     contentsChanged <- rb (changes contents)
     let contentsChangedSinceRender =
           stepper False
                   (fmap or
                         (collect (union (True <$ contentsChanged)
                                         (False <$ requiresRender))))
         requiresRender =
           whenE contentsChangedSinceRender (onRender env)
     traceM "Installing render loop"
     rb (reactimate
           (fmap (setData tNode . Just)
                 (contents <@ onRender env)))
     traceM "Marking"
     markedElem <- mark (castToNode tNode)
     traceM "All done"
     Francium (tell (pure [markedElem]))

tag :: Functor f
    => f b -> a -> f a
tag a b = b <$ a
{-# INLINE tag #-}

data MarkedNode =
  MarkedNode {nodeId :: Unique
             ,markedNode :: Node}

instance Eq MarkedNode where
  MarkedNode a _ == MarkedNode b _ = a == b

mark :: MonadIO m
     => Node -> m MarkedNode
mark node =
  do id_ <- liftIO newUnique
     return (MarkedNode id_
                        (castToNode node))

newTextElement :: MonadIO m
               => JSString -> m Text
newTextElement content =
  liftIO (do Just document <- currentDocument
             Just tnode <-
               createTextNode document content
             return tnode)

class Term arg result | result -> arg where
  term :: JSString -> arg -> result

instance (Frameworks t,x ~ Attribute t (),arg ~ Francium t a) => Term x (arg -> Francium t a) where
  term = mkElement

instance (Frameworks t,arg ~ Francium t a) => Term arg (Francium t a) where
  term name inner = mkElement name mempty inner

newtype Attribute t a =
  Attribute {runAttribute :: ReaderT (Node,Event t ()) (Moment t) a}

instance Functor (Attribute t) where
  fmap f (Attribute x) = Attribute (fmap f x)

instance Applicative (Attribute t) where
  pure = Attribute . pure
  Attribute f <*> Attribute x =
    Attribute (f <*> x)

instance Monad (Attribute t) where
  return = Attribute . return
  Attribute f >>= m =
    Attribute (f >>= runAttribute . m)

instance (a ~ ()) => Monoid (Attribute t a) where
  mempty = Attribute (return mempty)
  Attribute l `mappend` Attribute r =
    Attribute (liftA2 mappend l r)

domNode :: Francium t Node
domNode = Francium (asks parentNode)

newElement :: MonadIO m
           => JSString -> m Node
newElement element =
  liftIO (do Just document <- currentDocument
             Just domNode <-
               createElement document
                             (Just element)
             return (castToNode domNode))

mkElement
  :: Frameworks t
  => JSString -> Attribute t () -> Francium t a -> Francium t a
mkElement el (Attribute attributes) inner =
  mdo env <- Francium ask
      traceM "newElement"
      divElement <- newElement el
      traceM "runChildren"
      (a,children) <-
        rb (runWriterT
              (runReaderT (runFrancium inner)
                          env {parentNode = divElement}))
      traceM "runAttributes"
      _ <-
        rb (runReaderT attributes
                       (divElement,onRender env))
      traceM "initialChildren"
      initialChildren <- rb (initial children)
      traceM "append children"
      rb (liftIOLater
            (for_ initialChildren (appendChild divElement . Just . markedNode)))
      -- XXX TODO
      let lastRenderedChildren =
            pure initialChildren
      traceM "new children render loop"
      rb (reactimate
            (fmap (\(newChildren,oldChildren) ->
                     do traverse_ (appendChild divElement . Just . markedNode)
                                  (filter (not .
                                           (`elem` map nodeId oldChildren) .
                                           nodeId)
                                          newChildren)
                        traverse_ (removeChild divElement . Just . markedNode)
                                  (filter (not .
                                           (`elem` map nodeId newChildren) .
                                           nodeId)
                                          oldChildren))
                  (whenE (liftA2 (/=) children lastRenderedChildren)
                         (liftA2 (,) children lastRenderedChildren <@
                          (onRender env)))))
      traceM "mark container"
      markedElem <- liftIO (mark divElement)
      traceM "tell"
      Francium (tell (pure [markedElem]))
      traceM "ok"
      return a

mkAttribute :: (Eq v,Show v,Frameworks t)
            => (a -> v -> IO ())
            -> (Node -> a)
            -> Behavior t v
            -> Attribute t ()
mkAttribute f cast b =
  Attribute (ReaderT (\(node,renders) ->
                        do initialValue <- initial b
                           liftIOLater (f (cast node) initialValue)
                           contentsChanged <- changes b
                           let contentsChangedSinceRender =
                                 stepper False
                                         (fmap or
                                               (collect (union (True <$
                                                                contentsChanged)
                                                               (False <$
                                                                requiresRender))))
                               requiresRender =
                                 whenE contentsChangedSinceRender renders
                           reactimate
                             (fmap (f (cast node))
                                   (b <@ requiresRender))))

basicAttribute
  :: Frameworks t
  => JSString -> Behavior t JSString -> Attribute t ()
basicAttribute k =
  mkAttribute (\e -> setAttribute e k)
              castToElement

type_ :: Frameworks t
      => Behavior t JSString -> Attribute t ()
type_ = basicAttribute "type"

basicElement :: Term arg result
             => JSString -> arg -> result
basicElement el = term el

div_, li_, ul_ :: Term attrsOrChildren html
               => attrsOrChildren -> html
div_ = basicElement "div"
li_ = basicElement "li"
ul_ = basicElement "ul"

value_ :: Frameworks t
       => Behavior t JSString -> Attribute t ()
value_ b =
  let Attribute f =
        mkAttribute setValue
                    castToHTMLInputElement
                    (fmap Just b)
  in Attribute (do (node,renders) <- ask
                   lift (do runReaderT f
                                       (node,renders)
                            (input,fixInput) <- newEvent
                            eventListener <-
                              liftIO (eventListenerNew (const (fixInput ()) :: UIEvent -> IO ()))
                            liftIOLater
                              (addEventListener (castToHTMLInputElement node)
                                                ("input" :: JSString)
                                                (Just eventListener)
                                                False)
                            valueChanged <- changes b
                            reactimate
                              (fmap (setValue (castToHTMLInputElement node) .
                                     Just)
                                    (b <@ input))))

data InputElement t =
  InputElement {onInput :: Event t JSString
               ,onChange :: Event t JSString}

input_
  :: Frameworks t
  => Attribute t () -> Francium t (InputElement t)
input_ attrs =
  mkElement "input"
            attrs
            (do node <- domNode
                rb (do onInput_ <-
                         do (input,fireInput) <- newEvent
                            liftIOLater
                              (do eventListener <-
                                    eventListenerNew
                                      (const (getValue (castToHTMLInputElement node) >>=
                                              fireInput) :: UIEvent -> IO ())
                                  addEventListener (castToHTMLInputElement node)
                                                   ("input" :: JSString)
                                                   (Just eventListener)
                                                   False)
                            return input
                       onChange_ <-
                         do (changes,fireChange) <- newEvent
                            liftIOLater
                              (do eventListener <-
                                    (eventListenerNew
                                       (const (getValue (castToHTMLInputElement node) >>=
                                               fireChange) :: UIEvent -> IO ()))
                                  addEventListener (castToHTMLInputElement node)
                                                   ("change" :: JSString)
                                                   (Just eventListener)
                                                   False)
                            return changes
                       return InputElement {onInput =
                                              fmap (fromMaybe "") onInput_
                                           ,onChange =
                                              fmap (fromMaybe "") onChange_}))

francium
  :: (forall t. Frameworks t => Francium t a) -> IO ()
francium app =
  do (ah,tick) <- newAddHandler
     let app' :: Frameworks t
              => Moment t ()
         app' =
           do body <-
                liftIO (do Just document <- currentDocument
                           Just body <- getBody document
                           return body)
              render <- fromAddHandler ah
              let clock = fmap (/ 1000) render
              traceM "Building app"
              nodes <-
                execWriterT
                  (runReaderT
                     (runFrancium app)
                     (FranciumEnv (void render)
                                  (castToNode body)
                                  clock))
              startingNodes <- initial nodes
              liftIOLater
                (for_ (map markedNode startingNodes)
                      (appendChild (castToNode body) . Just))
     network <- compile app'
     actuate network
     forever (do waitForAnimationFrame
                 tick =<< performanceNow)

foreign import javascript unsafe
  "console.timeStamp($1)" timeStamp :: JSString -> IO ()

foreign import javascript unsafe
  "performance.now()"
  performanceNow :: IO Double

newtype CSS t a =
  CSS (Writer (Endo [(JSString,Behavior t JSString)]) a)
  deriving (Applicative,Functor,Monad,MonadFix)

instance (a ~ ()) => Monoid (CSS t a) where
  mempty = CSS (pure mempty)
  {-# INLINE mempty #-}
  CSS l `mappend` CSS r =
    CSS (liftA2 mappend l r)
  {-# INLINE mappend #-}

infixr 4 -:
(-:)
  :: JSString -> Behavior t JSString -> CSS t ()
k -: v = CSS (tell (Endo ((k,v) :)))
{-# INLINE (-:) #-}

style_ :: Frameworks t
       => CSS t x -> Attribute t ()
style_ (CSS styles) =
  Attribute (ReaderT (\(node,renders) ->
                        do Just cssDecl <-
                             getStyle (castToElement node)
                           for_ (appEndo (execWriter styles) [])
                                (\(k,v) ->
                                   do initialValue <- initial v
                                      liftIOLater
                                        (setProperty cssDecl
                                                     k
                                                     (Just initialValue)
                                                     ("" :: JSString))
                                      contentsChanged <- changes v
                                      let contentsChangedSinceRender =
                                            stepper False
                                                    (fmap or
                                                          (collect (union (True <$
                                                                           contentsChanged)
                                                                          (False <$
                                                                           requiresRender))))
                                          requiresRender =
                                            whenE contentsChangedSinceRender renders
                                      reactimate
                                        (fmap (\v' ->
                                                 setProperty cssDecl
                                                             k
                                                             (Just v')
                                                             ("" :: JSString))
                                              (v <@ requiresRender)))))

instance a ~ () => Monoid (Francium t a) where
  mempty = Francium (return ())
  Francium l `mappend` Francium r =
    Francium (l >> r)

instance IsString a => IsString (Behavior t a) where
  fromString = pure . fromString

instance (Frameworks t,a ~ ()) => IsString (Francium t a) where
  fromString = text_ . fromString

getClock :: Francium t (Event t Double)
getClock = Francium (asks appClock)

switchMany :: Frameworks t
           => Event t a
           -> (forall tLater. Frameworks tLater => a -> Francium tLater (Event tLater ()))
           -> Francium t ()
switchMany creations f =
  do env <- Francium ask
     trimmedRender <- rb (trimE (onRender env))
     trimmedAppClock <- rb (trimE (appClock env))
     moreChildren <-
       rb (execute (fmap (\a ->
                            FrameworksMoment
                              (do onRender' <- now trimmedRender
                                  appClock' <- now trimmedAppClock
                                  (done,children) <-
                                    runWriterT
                                      (runReaderT
                                         (runFrancium (f a))
                                         FranciumEnv {onRender = onRender'
                                                     ,appClock = appClock'
                                                     ,parentNode = parentNode env})
                                  trimB (switchB children (tag done (pure [])))))
                         creations))
     Francium (tell (switchB (pure []) moreChildren))
