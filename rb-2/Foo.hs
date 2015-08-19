{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Char
import Data.Maybe (fromMaybe)
import GHCJS.DOM.Types (IsGObject)
import GHCJS.DOM.HTMLButtonElement (castToHTMLButtonElement)
import Data.String
import Reactive.Banana
import Reactive.Banana.Frameworks
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Writer.Strict (WriterT(..), execWriterT)
import Control.Monad.Reader
import Control.Monad.Writer (MonadWriter(..))
import Control.Monad.IO.Class
import GHCJS.DOM.CSSStyleDeclaration
import GHCJS.DOM.CharacterData (setData)
import GHCJS.DOM.Element (getStyle, setAttribute)
import GHCJS.DOM.EventTarget (addEventListener)
import GHCJS.DOM.EventTargetClosures
import GHCJS.DOM.HTMLInputElement
import GHCJS.DOM.Node
import GHCJS.DOM.Types (UIEvent, castToElement)
import GHCJS.Types
import Data.JSString (pack, unpack)
import Data.Unique
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Document (createElement, createTextNode, getBody)
import GHCJS.DOM.Text (Text)
import GHCJS.DOM.Types (Node, castToNode)
import Data.Coerce
import Data.Monoid
import Data.Foldable
import JavaScript.Web.AnimationFrame

data FranciumEnv t =
  FranciumEnv {onRender :: Event t ()
              ,onDestroy :: Event t ()
              ,parentNode :: Node
              ,appClock :: Behavior t Double}

newtype Francium t a =
  Francium {runFrancium :: ReaderT (FranciumEnv t) (WriterT (Behavior t [MarkedNode]) (Moment t)) a}

instance Monad (Francium t) where
  return a = Francium (return a)
  Francium a >>= f = Francium (a >>= runFrancium . f)

instance Frameworks t => MonadIO (Francium t) where
  liftIO m = Francium (liftIO m)

instance Applicative (Francium t) where
  pure = return
  (<*>) = ap

instance Functor (Francium t) where
  fmap f (Francium m) = Francium (fmap f m)

instance MonadFix (Francium t) where
  mfix f = Francium (mfix (runFrancium . f))

rb :: Frameworks t => Moment t a -> Francium t a
rb m = Francium (lift (lift m))

instance Monoid a => Monoid (Behavior t a) where
  mempty = pure mempty
  mappend = liftA2 mappend

text_ :: Frameworks t => Behavior t JSString -> Francium t ()
text_ contents =
  do initialContent <- rb (initial contents)
     tNode <- newTextElement initialContent
     env <- Francium ask
     contentsChanged <- rb (changes contents)
     let contentsChangedSinceRender =
           stepper False
                   (fmap or
                         (collect (union (True <$ contentsChanged)
                                         (False <$ requiresRender))))
         requiresRender =
           whenE contentsChangedSinceRender (onRender env)
     rb $
       do trimmedContents <- trimB contents
          execute (tag requiresRender
                       (FrameworksMoment
                          (setData tNode . Just =<<
                                valueB =<< now trimmedContents)))
     markedElem <- mark (castToNode tNode)
     Francium (tell (pure [markedElem]))

tag :: Functor f => f b -> a -> f a
tag a b = b <$ a


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

instance (Frameworks t, x ~ Attribute t (),arg ~ Francium t a) => Term x (arg -> Francium t a) where
  term = mkElement

instance (Frameworks t, arg ~ Francium t a) => Term arg (Francium t a) where
  term name inner = mkElement name mempty inner

newtype Attribute t a =
  Attribute (ReaderT (Node,Event t (),Event t ()) (Moment t) a)

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
  do env <- Francium ask
     divElement <- liftIO (newElement el)
     (a,children) <-
       rb (runWriterT
             (runReaderT (runFrancium inner)
                         env {parentNode = divElement}))
     _ <-
       rb (runReaderT attributes
                      (divElement,onRender env,onDestroy env))
     initialChildren <- rb (valueB children)
     -- liftIO (timeStamp ("Adding children to " <> el))
     rb (liftIOLater (for_ initialChildren (appendChild divElement . Just . markedNode)))
     contentsChanged <- rb (changes children)
     let contentsChangedSinceRender =
           stepper False
                   (union (True <$ contentsChanged)
                          (False <$ requiresRender))
         requiresRender =
           whenE contentsChangedSinceRender (onRender env)
     rb $
       do trimmedContents <- trimB children
          execute (tag requiresRender
                       (FrameworksMoment
                          (liftIO .
                           traverse (appendChild divElement . Just . markedNode) =<<
                           valueB =<< now trimmedContents)))
     markedElem <- liftIO (mark divElement)
     Francium (tell (pure [markedElem]))
     return a

mkAttribute :: (Eq v,Show v,Frameworks t)
            => (a -> v -> IO ())
            -> (Node -> a)
            -> Behavior t v
            -> Attribute t ()
mkAttribute f cast b =
  Attribute (ReaderT (\(node,renders,destroyed) ->
                        do initialValue <- valueB b
                           -- sync (timeStamp "Initially setting attributes")
                           liftIOLater (f (cast node) initialValue)
                           contentsChanged <- changes b
                           let contentsChangedSinceRender =
                                 stepper False
                                         (union (True <$ contentsChanged)
                                                (False <$ requiresRender))
                               requiresRender =
                                 whenE contentsChangedSinceRender renders
                           do trimmedContents <- trimB b
                              execute (tag requiresRender
                                           (FrameworksMoment
                                              (liftIO . f (cast node) =<<
                                               valueB =<< now trimmedContents)))
                           b `seq` return ()))

basicAttribute
  :: Frameworks t
  => JSString -> Behavior t JSString -> Attribute t ()
basicAttribute k =
  mkAttribute (\e -> setAttribute e k)
              castToElement

type_ :: Frameworks t => Behavior t JSString -> Attribute t ()
type_ = basicAttribute "type"

basicElement :: Term arg result
             => JSString -> arg -> result
basicElement el = term el

div_, li_, ul_ :: Term attrsOrChildren html
               => attrsOrChildren -> html
div_ = basicElement "div"
li_ = basicElement "li"
ul_ = basicElement "ul"

data ButtonElement t =
  ButtonElement {onClick :: Event t UIEvent
                ,onMouseEnter :: Event t UIEvent
                ,onMouseLeave :: Event t UIEvent}

button_
  :: Frameworks t
  => Attribute t () -> Francium t a -> Francium t (ButtonElement t)
button_ attrs contents =
  basicElement
    "button"
    attrs
    (do _ <- contents
        node <- domNode
        ButtonElement <$>
          domEvStream (castToHTMLButtonElement node)
                      "click" <*>
          domEvStream (castToHTMLButtonElement node)
                      "mouseenter" <*>
          domEvStream (castToHTMLButtonElement node)
                      "mouseleave")

value_ :: Frameworks t
       => Behavior t JSString -> Attribute t ()
value_ b =
  let Attribute f =
        mkAttribute setValue
                    castToHTMLInputElement
                    (fmap Just b)
  in Attribute (do (node,renders,destroyed) <- ask
                   lift (do runReaderT f
                                       (node,renders,destroyed)
                            (input,fixInput) <- newEvent
                            eventListener <-
                              liftIO (eventListenerNew (const (fixInput ()) :: UIEvent -> IO ()))
                            liftIOLater
                              (addEventListener (castToHTMLInputElement node)
                                                ("input" :: JSString)
                                                (Just eventListener)
                                                False)
                            -- _ <-
                            --   plan (tag destroyed (sync (eventListenerRelease eventListener)))
                            do valueChanged <- changes b
                               trimmedValue <- trimB b
                               execute (tag input
                                            (FrameworksMoment
                                               (now trimmedValue >>= valueB >>=
                                                liftIO .
                                                setValue (castToHTMLInputElement node) .
                                                Just)))
                            return ()))

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
                destroyed <-
                  Francium (asks onDestroy)
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
                            -- _ <-
                            --   plan (tag destroyed (liftIO (eventListenerRelease eventListener)))
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
                            -- _ <-
                            --   plan (tag destroyed (liftIO (eventListenerRelease eventListener)))
                            return changes
                       return InputElement {onInput =
                                              fmap (fromMaybe "") onInput_
                                           ,onChange =
                                              fmap (fromMaybe "") onChange_}))

francium :: (forall t. Frameworks t => Behavior t Double -> Francium t a) -> IO ()
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
              perfNow <- liftIO performanceNow
              let clock = stepper perfNow render
              nodes <-
                execWriterT
                  (runReaderT
                     (runFrancium (app clock))
                     (FranciumEnv (void render)
                                  never
                                  (castToNode body)
                                  (pure 0)))
              (MarkedNode _ text:_) <- valueB nodes
              liftIO (appendChild (castToNode body)
                                  (Just text))
              return ()
     network <- compile app'
     actuate network
     forever (do waitForAnimationFrame
                 t' <- performanceNow
                 tick t')

domEvStream
  :: (IsGObject a, Frameworks t)
  => a -> String -> Francium t (Event t UIEvent)
domEvStream node event =
  do -- destroyed <- Francium (asks onDestroy)
     (evstream,fire) <- rb newEvent
     eventListener <-
       liftIO (eventListenerNew fire)
     addEventListener (castToHTMLButtonElement node)
                      event
                      (Just eventListener)
                      False
     -- _ <-
     --   plan (tag destroyed (liftIO (eventListenerRelease eventListener)))
     return evstream

main :: IO ()
main =
  francium (\clock ->
              div_ $
              mdo b <-
                    button_ (type_ (pure "text"))
                            (text_ (pure "Click me"))
                  let clickCount =
                        accumB 0 (tag (onClick b) (+ 1))
                  i <-
                    input_ (value_ (stepper "" (onInput i)))
                  text_ (fmap (fromString . show) clock)
                  ul_ (li_ (text_ (fmap (fromString . show)
                                        (stepper ""
                                                 (fmap (pack .
                                                        fmap toUpper . unpack)
                                                       (onChange i)))))))

foreign import javascript unsafe
  "performance.now()"
  performanceNow :: IO Double
