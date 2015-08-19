{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Mem
import Numeric.AD (Mode, Scalar, auto)
import Numeric.AD.Rank1.Forward.Double (diff')
import Control.Monad.Trans.Class
import Reactive.Banana
import Reactive.Banana.Frameworks
import GHCJS.DOM.ClientRect (getTop, getLeft)
import Control.Monad.IO.Class
import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Strict
import Data.Bool
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.String (fromString)
import Francium hiding (main)
import GHCJS.DOM
import GHCJS.DOM.Document (getBody)
import GHCJS.DOM.Element
       (setAttribute, castToElement, getScrollLeft, getScrollTop,
        getOffsetHeight, getOffsetWidth, getBoundingClientRect)
import GHCJS.DOM.HTMLButtonElement (castToHTMLButtonElement)
import GHCJS.DOM.Event hiding (Event)
import GHCJS.DOM.EventTarget (addEventListener)
import GHCJS.DOM.EventTargetClosures
       (eventListenerNew, eventListenerRelease)
import GHCJS.DOM.UIEvent (UIEvent, getPageX, getPageY)
import GHCJS.DOM.Types (IsGObject)
import GHCJS.Types
import Debug.Trace

import qualified CSS

--------------------------------------------------------------------------------

data AvatarContents =
  AvatarImg String

data AvatarConfig t =
  AvatarConfig {_avatarSize :: Behavior t Integer
               ,_avatarBackgroundColor :: Behavior t JSString
               ,_avatarBorderRadius :: Behavior t Double}

makeLenses ''AvatarConfig

defaultAvatarConfig :: AvatarConfig t
defaultAvatarConfig =
  AvatarConfig {_avatarSize = 40
               ,_avatarBackgroundColor = CSS.white
               ,_avatarBorderRadius = 50}

avatar
  :: Frameworks t
  => State (AvatarConfig t) x -> AvatarContents -> Francium t ()
avatar configurator contents =
  div_ (style_ containerStyle)
       (case contents of
          AvatarImg imageUrl ->
            img_ (do src_ (pure imageUrl)
                     style_ imageStyle)
          _ ->
            div_ (style_ (do CSS.position CSS.absolute
                             CSS.top (0 CSS.@@ CSS.px)
                             CSS.left (0 CSS.@@ CSS.px)
                             CSS.width (0 CSS.@@ CSS.pct)
                             CSS.height (0 CSS.@@ CSS.pct)))
                 mempty)
  where config =
          execState configurator defaultAvatarConfig
        containerStyle =
          do "userSelect" -: "none"
             imageStyle
             CSS.backgroundColor (config ^. avatarBackgroundColor)
             CSS.display CSS.inlineBlock
             CSS.position CSS.relative
             CSS.textAlign CSS.center
             CSS.lineHeight (config ^. avatarSize CSS.@@ CSS.px)
             CSS.fontSize
               (fmap (\sz -> sz `div` 2 + 4)
                     (config ^. avatarSize) CSS.@@
                CSS.px)
        imageStyle =
          do CSS.height (config ^. avatarSize CSS.@@ CSS.px)
             CSS.width (config ^. avatarSize CSS.@@ CSS.px)
             "border-radius" -:
               fmap ((<> "px") . fromString . show)
                    (config ^. avatarBorderRadius)

img_ :: Frameworks t => Attribute t () -> Francium t ()
img_ attrs = basicElement "img" attrs mempty

src_ :: Frameworks t => Behavior t String -> Attribute t ()
src_ =
  mkAttribute (flip setAttribute ("src" :: JSString))
              castToElement

--------------------------------------------------------------------------------

data FullscreenOverlayConfig t =
  FullscreenOverlayConfig {_fullscreenOverlayLockScrolling :: Bool
                          ,_fullscreenOverlayActive :: Behavior t Bool}

makeLenses ''FullscreenOverlayConfig

-- fullscreenOverlay
--   :: Frameworks t => State (FullscreenOverlayConfig t) x -> Francium t ()
-- fullscreenOverlay mkFsoConfig =
--   do let visibilityChanged =
--            toChanges (fsoConfig ^. fullscreenOverlayActive)
--      clock <- getClock
--      overlayOpacity <-
--        sample (linearInterpolator' clock
--                                    0
--                                    0.4
--                                    (fmap (bool 0 1) visibilityChanged))
--      observe (fsoConfig ^. fullscreenOverlayActive)
--              (\case
--                 False -> return ()
--                 True ->
--                   div_ (style_ (do CSS.position CSS.fixed
--                                    CSS.height (100 CSS.@@ CSS.pct)
--                                    CSS.width (100 CSS.@@ CSS.pct)
--                                    CSS.zIndex 9
--                                    CSS.top (0 CSS.@@ CSS.px)
--                                    CSS.left (0 CSS.@@ CSS.px)
--                                    "opacity" -: fmap (fromString . show) overlayOpacity
--                                    "background-color" -: "rgba(0,0,0,0.541)"
--                                    "transform" -: "translateZ(0px)"))
--                        mempty)
--   where fsoConfig =
--           execState mkFsoConfig
--                     FullscreenOverlayConfig {_fullscreenOverlayLockScrolling = True
--                                             ,_fullscreenOverlayActive =
--                                                pure False}

def :: State config ()
def = return ()

--------------------------------------------------------------------------------

integrate
  :: Event t Double -> Behavior t Double -> Behavior t Double
integrate t b = sumB ((*) <$> b <@> diffE t)
  where diffE = withPrevEWith (-)
        withPrevEWith f e =
          filterJust . fst . mapAccum Nothing $ g <$> e
          where g y Nothing = (Nothing,Just y)
                g y (Just x) =
                  (Just (f y x),Just y)
        sumB = accumB 0 . fmap (+)

cubicBezier
  :: (Double,Double) -> (Double,Double) -> Double -> Double -> Double
cubicBezier (p2x,p2y) (p3x,p3y) dur x =
  let epsilon = 1 / (200 * dur)
      cubicBezierX :: (Mode a, Scalar a ~ Double) => a -> a
      cubicBezierX t =
        ((ax * t + bx) * t + cx) * t
      ax,bx,cx :: (Mode a, Scalar a ~ Double) => a
      cx = 3 * auto p2x
      bx = 3 * (auto p3x - auto p2x) - cx
      ax = 1 - cx - bx
      cubicBezierY t =
        ((ay * t + by) * t + cy) * t
      cy = 3 * auto p2y
      by = 3 * (auto p3y - auto p2y) - cy
      ay = 1 - cy - by
      findZero f =
        let go x =
              let (y,y') = diff' f x
                  xn = x - y / y'
              in x :
                 if x == xn
                    then []
                    else go xn
        in go
  in cubicBezierY
       (head (dropWhile ((> epsilon) . abs . subtract x . cubicBezierX)
                        (findZero (\t -> cubicBezierX t - auto x) x)))

interpolator
  :: Event t Double -> Double -> Behavior t Double
interpolator clock duration =
  fmap (cubicBezier (0.23,1)
                    (0.32,1)
                    (duration * 1000) .
        min 1 . max 0)
       (integrate clock (pure (1 / duration)))

lerp :: Double -> Double -> Double -> Double
lerp a b x = a + (b - a) * x

linearInterpolator'
  :: Event t Double
  -> Double
  -> Double
  -> Event t Double
  -> Moment t (Behavior t Double)
linearInterpolator' clock v0 duration transitions =
  do trimmedClock <- trimE clock
     let animation start target =
           anyMoment (do clock' <- now trimmedClock
                         return (fmap (lerp start target)
                                      (interpolator clock' duration)))
         val =
           switchB (pure v0)
                   (animation <$> val <@> transitions)
     pure val

--------------------------------------------------------------------------------

data PaperShape
  = PaperRounded
  | PaperCircle
  | PaperRectangle

data PaperConfig =
  PaperConfig {_paperShape :: PaperShape
              ,_paperDepth :: Int}

makeLenses ''PaperConfig

paper
  :: Frameworks t
  => State PaperConfig x -> Francium t () -> Francium t ()
paper mkPaperConfig =
  div_ (style_ (do CSS.backgroundColor CSS.white
                   "box-sizing" -: "border-box"
                   "box-shadow" -:
                     pure (head (drop (paperConfig ^. paperDepth) boxShadows))
                   "border-radius" -:
                     pure (case paperConfig ^. paperShape of
                             PaperRounded -> "2px"
                             PaperCircle -> "50%"
                             _ -> "0")))
  where paperConfig =
          execState mkPaperConfig
                    PaperConfig {_paperShape = PaperRounded
                                ,_paperDepth = 1}
        boxShadows =
          ["0 1px 6px rgba(0, 0, 0, 0.12), 0 1px 4px rgba(0, 0, 0, 0.24)"
          ,"0 3px 10px rgba(0, 0, 0, 0.16), 0 3px 10px rgba(0, 0, 0, 0.23)"
          ,"0 10px 30px rgba(0, 0, 0, 0.19), 0 6px 10px rgba(0, 0, 0, 0.23)"
          ,"0 14px 45px rgba(0, 0, 0, 0.25), 0 10px 18px rgba(0, 0, 0, 0.22)"
          ,"0 19px 60px rgba(0, 0, 0, 0.30), 0 15px 20px rgba(0, 0, 0, 0.22)"]

-- --------------------------------------------------------------------------------

-- data DialogConfig =
--   DialogConfig {_dialogOpen :: Behavior Bool}

-- makeLenses ''DialogConfig

-- dialog :: State DialogConfig a -> Francium ()
-- dialog mkDialogConfig =
--   do fullscreenOverlay (fullscreenOverlayActive .= (dialogConfig ^. dialogOpen))
--      switchMany
--        (tag (edges (dialogConfig ^. dialogOpen))
--             (div_ (style_ (do CSS.position CSS.fixed
--                               CSS.zIndex 10
--                               CSS.top (0 CSS.@@ CSS.px)
--                               CSS.left (0 CSS.@@ CSS.px)
--                               CSS.width (100 CSS.@@ CSS.pct)
--                               CSS.height (100 CSS.@@ CSS.pct)
--                               "box-sizing" -: "border-box"))
--                   newDialog))
--   where dialogHeader =
--           h3_ (style_ (do CSS.margin (0 CSS.@@ CSS.px)
--                           CSS.padding (24 CSS.@@ CSS.px)
--                                       (24 CSS.@@ CSS.px)
--                                       (0 CSS.@@ CSS.px)
--                                       (24 CSS.@@ CSS.px)
--                           CSS.fontSize (24 CSS.@@ CSS.px)
--                           CSS.lineHeight (32 CSS.@@ CSS.px)))
--               (text_ "static")
--         dialogBody =
--           div_ (style_ (CSS.padding (24 CSS.@@ CSS.px))) "Dialog"
--         renderDialog =
--           paper (paperDepth .= 4)
--                 (do dialogHeader
--                     dialogBody)
--         newDialog :: Francium (Event ())
--         newDialog =
--           do clock <- getClock
--              entranceAnimation <-
--                sample (interpolator clock 0.45)
--              dropIn entranceAnimation renderDialog
--              return never
--         dropIn
--           :: Behavior Double -> Francium () -> Francium ()
--         dropIn animationAmount content =
--           div_ (style_ (do "box-sizing" -: "border-box"
--                            "opacity" -: fmap (fromString . show) animationAmount
--                            CSS.position CSS.relative
--                            CSS.width (75 CSS.@@ CSS.pct)
--                            CSS.margin (0 CSS.@@ CSS.px)
--                                       CSS.auto
--                            CSS.zIndex 10
--                            "transform" -:
--                              (fmap (("translate3d(0px," <>) .
--                                     (<> "px,0px)") . fromString . show . lerp 0 64)
--                                    animationAmount)))
--                content
--         dialogConfig =
--           execState mkDialogConfig
--                     DialogConfig {_dialogOpen =
--                                     pure False}

-- h3_ :: Term () attrsOrChildren html
--     => attrsOrChildren -> html
-- h3_ = basicElement "h3"

--------------------------------------------------------------------------------

data ButtonConfig =
  ButtonConfig {}

makeLenses ''ButtonConfig

data Button t =
  Button {buttonClicks :: Event t ()}

button
  :: Frameworks t
  => State ButtonConfig () -> Francium t () -> Francium t (Button t)
button mkButtonConfig content =
  mdo let hovering =
            union (True <$ onMouseEnter buttonElement)
                  (False <$ onMouseLeave buttonElement)
      clock <- getClock
      backgroundColor <-
        fmap (fmap (\t ->
                      (round (lerp 0 255 t) :: Int,lerp 0.14902 1 t)))
             (rb (linearInterpolator' clock
                                      1
                                      0.4
                                      (fmap (bool 1 0) hovering)))
      rb (reactimate (fmap print hovering))
      buttonElement <-
        button_ (style_ (do "border" -: "10px"
                            "background" -: "none"
                            "box-sizing" -: "border-box"
                            "appearance" -: "button"
                            "cursor" -: "pointer"
                            "text-decoration" -: "none"
                            "outline" -: "none"
                            "letter-spacing" -: "0"
                            "font-family" -: "Roboto"
                            "font-weight" -: "500"
                            "border-radius" -: "2px"
                            "user-select" -: "none"
                            "position" -: "relative"
                            "overflow" -: "hidden"
                            "background-color" -:
                              fmap (\(diffuse,alpha) ->
                                      rgba diffuse diffuse diffuse alpha)
                                   backgroundColor
                            CSS.display CSS.inlineBlock
                            CSS.fontSize (14 CSS.@@ CSS.px)
                            CSS.textTransform CSS.uppercase
                            CSS.position CSS.relative
                            CSS.overflow CSS.hidden
                            CSS.lineHeight (36 CSS.@@ CSS.px)
                            CSS.padding (0 CSS.@@ CSS.px)
                            CSS.margin (0 CSS.@@ CSS.px)
                            "transform" -: "translate3d(0, 0, 0)"))
                (do div_ (style_ (do CSS.height (100 CSS.@@ CSS.pct)
                                     CSS.width (100 CSS.@@ CSS.pct)
                                     CSS.position CSS.absolute
                                     CSS.top (0 CSS.@@ CSS.px)
                                     CSS.left (0 CSS.@@ CSS.px)
                                     CSS.overflow CSS.hidden))
                         (switchMany
                            (onClick buttonElement)
                            (\uiEvent ->
                               liftIO (rippleCSS uiEvent) >>= ripple))
                    span_ (style_ (CSS.padding (0 CSS.@@ CSS.px)
                                               (16 CSS.@@ CSS.px)))
                          content)
      return Button {buttonClicks =
                       void (onClick buttonElement)}
  where buttonConfig =
          execState mkButtonConfig ButtonConfig {}
        rippleCSS :: UIEvent -> IO (CSS t ())
        rippleCSS uiEvent =
          do Just el <-
               fmap (fmap castToElement)
                    (getTarget uiEvent)
             elHeight <- getOffsetHeight el
             elWidth <- getOffsetWidth el
             Just document <- currentDocument
             Just body <- getBody document
             Just boundingClientRect <- getBoundingClientRect el
             offset <-
               liftA2 (,)
                      (liftA2 (+)
                              (getTop boundingClientRect)
                              (fmap fromIntegral (getScrollTop body)))
                      (liftA2 (+)
                              (getLeft boundingClientRect)
                              (fmap fromIntegral (getScrollLeft body)))
             pageX <-
               fmap fromIntegral (getPageX uiEvent)
             pageY <-
               fmap fromIntegral (getPageY uiEvent)
             let pointerX =
                   realToFrac (pageX - (snd offset))
                 pointerY =
                   realToFrac (pageY - (fst offset))
                 calcDiag a b =
                   sqrt ((a * a) + (b * b))
                 topLeftDiag =
                   calcDiag pointerX pointerY
                 topRightDiag =
                   calcDiag (elWidth - pointerX) pointerY
                 botRightDiag =
                   calcDiag (elWidth - pointerX)
                            (elHeight - pointerY)
                 botLeftDiag =
                   calcDiag pointerX (elHeight - pointerY)
                 rippleRadius =
                   maximum [topLeftDiag,topRightDiag,botRightDiag,botLeftDiag]
                 rippleSize = rippleRadius * 2
             return (do CSS.height (pure rippleSize CSS.@@ CSS.px)
                        CSS.width (pure rippleSize CSS.@@ CSS.px)
                        CSS.top (pure (pointerY - rippleRadius) CSS.@@ CSS.px)
                        CSS.left (pure (pointerX - rippleRadius) CSS.@@ CSS.px))

rgba :: (Show a,Integral a,Num a,Num b,Show b)
     => a -> a -> a -> b -> JSString
rgba r g b a =
  fromString
    ("rgba(" ++
     show r ++ "," ++ show g ++ "," ++ show b ++ "," ++ show a ++ ")")

span_ :: Term arg result
      => arg -> result
span_ = basicElement "span"

data ButtonElement t =
  ButtonElement {onClick :: Event t UIEvent
                ,onMouseEnter :: Event t UIEvent
                ,onMouseLeave :: Event t UIEvent}

button_
  :: Frameworks t
  => Attribute t () -> Francium t a -> Francium t (ButtonElement t)
button_ attrs contents = do
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

domEvStream
  :: (Frameworks t, IsGObject a)
  => a -> String -> Francium t (Event t UIEvent)
domEvStream node event =
  do (evstream,fire) <- rb newEvent
     rb (liftIOLater
           (do eventListener <- eventListenerNew fire
               addEventListener (castToHTMLButtonElement node)
                                event
                                (Just eventListener)
                                False))
     -- _ <-
     --   plan (tag destroyed (liftIO (eventListenerRelease eventListener)))
     return evstream

--------------------------------------------------------------------------------
ripple :: Frameworks t => CSS t () -> Francium t (Event t ())
ripple extraStyle =
  do clock <- getClock
     let opacityAnimation = interpolator clock 2
     let scaleAnimation = interpolator clock 1
     div_ (style_ (do CSS.position CSS.absolute
                      CSS.top (-30 CSS.@@ CSS.px)
                      CSS.left (-3 CSS.@@ CSS.px)
                      CSS.height (100 CSS.@@ CSS.px)
                      CSS.width (100 CSS.@@ CSS.px)
                      "opacity" -:
                        fmap (fromString . show . lerp 0.16 0) opacityAnimation
                      "border-radius" -: "50%"
                      "background-color" -: "rgba(0,0,0,222)"
                      "transform" -:
                        fmap (("scale(" <>) .
                              (<> ")") . fromString . show . lerp 0 1)
                             scaleAnimation
                      extraStyle))
          mempty
     return (void (filterE (> 2) (integrate clock 1 <@ clock)))

--------------------------------------------------------------------------------

main :: IO ()
main = francium materialUIDemo

materialUIDemo :: Frameworks t => Francium t ()
materialUIDemo =
  getClock >>= text_ . fmap (fromString . show) . stepper 0
--   do render <- Francium (asks onRender)
--      let contents = accumB 0 . fmap (const (+1)) $ render
--      --text_ contents
--      rb $ reactimate $ fmap print (contents <@ render)

  -- mdo paper def
  --          (mdo traceM "link_"
  --               link_ (do href_ "http://fonts.googleapis.com/css?family=Roboto:400,300,500"
  --                         rel_ "stylesheet"
  --                         type_ "text/css")
  --               b <-
  --                 button_ mempty
  --                         (text_ (fmap (fromString . show)
  --                                      (stepper False (tag (onClick b) True))))
  --               traceM "avatar"
  --               avatar (avatarBackgroundColor .= "salmon")
  --                      (AvatarImg "http://www.unixstickers.com/image/cache/data/stickers/haskell/Haskell.sh-600x600.png")
  --               traceM "showDialog"
  --               -- ripple (return ())
  --               showDialogButton <-
  --                 button def "Show Dialog"
  --               -- dialogIsOpen <-
  --               --   sample (fromChanges False
  --               --                       (True <$ buttonClicks showDialogButton))
  --               traceM "switchMany"
  --               switchMany
  --                 (buttonClicks showDialogButton)
  --                 (const (do div_ "Ouch!"
  --                            return never))
  --               -- dialog (dialogOpen .= dialogIsOpen))
  --               traceM "getClock"
  --               clock <- getClock
  --               traceM "clock"
  --               text_ (fmap (fromString . show)
  --                           (accumB 0 (fmap (+) clock)))
  --               return ())

link_ :: Frameworks t => Attribute t () -> Francium t ()
link_ attrs = basicElement "link" attrs mempty

rel_, href_ :: Frameworks t => Behavior t JSString -> Attribute t ()
href_ = basicAttribute "href"
rel_ = basicAttribute "rel"
