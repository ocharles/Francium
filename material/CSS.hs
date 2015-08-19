{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module CSS
       (absolute, static, fixed, Position, position, top, (@@), px, left,
        width, pct, height, backgroundColor, display, inlineBlock,
        relative, textAlign, right, center, justify, lineHeight, fontSize,
        white, zIndex, margin, padding, auto, textTransform, uppercase,
        capitalize, lowercase, overflow, hidden)
       where

import Reactive.Banana
import Data.JSString (JSString)
import Data.String (fromString)
import Control.Applicative
import Data.Monoid
import Data.Coerce
import Francium

--------------------------------------------------------------------------------
coerceStr :: Coercible JSString a => JSString -> a
coerceStr = coerce

class Coercible JSString a => Absolute a where
  absolute :: Behavior t a
  absolute = pure (coerceStr "absolute")
  {-# INLINE absolute #-}

class Coercible JSString a => Fixed a where
  fixed :: Behavior t a
  fixed = pure (coerceStr "fixed")
  {-# INLINE fixed #-}

class Coercible JSString a => Static a where
  static :: Behavior t a
  static = pure (coerceStr "static")
  {-# INLINE static #-}

class Coercible JSString a => Relative a where
  relative :: Behavior t a
  relative = pure (coerceStr "relative")
  {-# INLINE relative #-}

class Coercible JSString a => Auto a where
  auto :: Behavior t a
  auto = pure (coerceStr "auto")
  {-# INLINE auto #-}

--------------------------------------------------------------------------------
newtype Position = Position JSString

instance Absolute Position
instance Fixed Position
instance Static Position
instance Relative Position

position :: Behavior t Position -> CSS t ()
position v = "position" -: fmap coerce v
{-# INLINE position #-}

--------------------------------------------------------------------------------
infixl 1 @@
(@@) :: a -> (a -> b) -> b
x @@ f = f x
{-# INLINE (@@) #-}

instance Num a => Num (Behavior t a) where
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = pure . fromInteger
  {-# INLINE fromInteger #-}
  negate = fmap negate
  {-# INLINE negate #-}

instance Fractional a => Fractional (Behavior t a) where
  fromRational = pure . fromRational
  {-# INLINE fromRational #-}
  recip = fmap recip
  {-# INLINE recip #-}

--------------------------------------------------------------------------------
class Coercible JSString a => Length a where
  mkLength :: Behavior t JSString -> Behavior t a
  mkLength = fmap coerce
  {-# INLINE mkLength #-}

px :: (Num a, Show a, Length css) => Behavior t a -> Behavior t css
px = mkLength . fmap (coerceStr . (<> "px") . fromString . show) -- TODO Data.JSString probably exports something better
{-# INLINE px #-}

--------------------------------------------------------------------------------
class Coercible JSString a => Percentage a where
  mkPercentage :: Behavior t JSString -> Behavior t a
  mkPercentage = fmap coerce
  {-# INLINE mkPercentage #-}

pct :: (Num a, Show a, Percentage css) => Behavior t a -> Behavior t css
pct = mkPercentage . fmap (coerceStr . (<> "%") . fromString . show)
{-# INLINE pct #-}

--------------------------------------------------------------------------------
newtype Top = Top JSString

instance Length Top
instance Percentage Top

top :: Behavior t Top -> CSS t ()
top v = "top" -: fmap coerce v
{-# INLINE top #-}

--------------------------------------------------------------------------------
newtype Left = Left JSString

instance Length Left
instance Percentage Left

class ValueOrKeyword a where
  valueOrKeyword :: JSString -> a

instance ValueOrKeyword (Behavior t JSString) where
  valueOrKeyword kw = pure (coerceStr kw)
  {-# INLINE valueOrKeyword #-}

instance (b ~ (), x ~ Left, t ~ t') => ValueOrKeyword (Behavior t x -> CSS t' b) where
  valueOrKeyword kw v = kw -: fmap coerce v
  {-# INLINE valueOrKeyword #-}

left :: ValueOrKeyword a => a
left = valueOrKeyword "left"
{-# INLINE left #-}

--------------------------------------------------------------------------------
newtype Width = Width JSString

instance Length Width
instance Percentage Width

width :: Behavior t Width -> CSS t ()
width v = "width" -: fmap coerce v
{-# INLINE width #-}

--------------------------------------------------------------------------------
newtype Height = Height JSString

instance Length Height
instance Percentage Height

height :: Behavior t Height -> CSS t ()
height v = "height" -: fmap coerce v
{-# INLINE height #-}

--------------------------------------------------------------------------------
newtype BackgroundColor = BackgroundColor JSString

backgroundColor :: Behavior t JSString -> CSS t ()
backgroundColor v = "background-color" -: fmap coerceStr v
{-# INLINE backgroundColor #-}

--------------------------------------------------------------------------------
-- TODO Values
newtype Display = Display JSString

inlineBlock :: Behavior t Display
inlineBlock = pure (Display "inline-block")
{-# INLINE inlineBlock #-}

display :: Behavior t Display -> CSS t ()
display v = "display" -: fmap coerce v
{-# INLINE display #-}

--------------------------------------------------------------------------------
newtype TextAlign = TextAlign JSString

right, center, justify :: Behavior t TextAlign
right = pure (coerceStr "right")
center = pure (coerceStr "center")
justify = pure (coerceStr "justify")
{-# INLINE right #-}
{-# INLINE center #-}
{-# INLINE justify #-}

textAlign :: Behavior t TextAlign -> CSS t ()
textAlign v = "display" -: fmap coerce v
{-# INLINE textAlign #-}

instance ValueOrKeyword (Behavior t TextAlign) where
  valueOrKeyword = pure . coerce
  {-# INLINE valueOrKeyword #-}

--------------------------------------------------------------------------------
newtype LineHeight = LineHeight JSString

instance Length LineHeight
instance Percentage LineHeight

lineHeight :: Behavior t LineHeight -> CSS t ()
lineHeight v = "line-height" -: fmap coerce v
{-# INLINE lineHeight #-}

--------------------------------------------------------------------------------
newtype FontSize = FontSize JSString

instance Length FontSize
instance Percentage FontSize

fontSize :: Behavior t FontSize -> CSS t ()
fontSize v = "fontSize" -: fmap coerce v
{-# INLINE fontSize #-}

--------------------------------------------------------------------------------
white :: Behavior t JSString
white = pure "white"
{-# INLINE white #-}

--------------------------------------------------------------------------------
zIndex :: (Integral a, Show a) => Behavior t a -> CSS t ()
zIndex v = "z-index" -: fmap (fromString . show) v
{-# INLINE zIndex #-}

--------------------------------------------------------------------------------
newtype Margin = Margin JSString

instance Length Margin
instance Percentage Margin

class MarginOverload a where
  margin :: a

instance (a ~ Margin,b ~ (), t ~ t') => MarginOverload (Behavior t a -> CSS t' b) where
  margin v = "margin" -: fmap coerce v
  {-# INLINE margin #-}

instance (a ~ Margin,b ~ a,c ~ (), t ~ t', t' ~ t'') => MarginOverload (Behavior t a -> Behavior t' b -> CSS t'' c) where
  margin a b =
    "margin" -:
    (liftA2 (\a' b' -> coerce a' <> " " <> coerce b') a b)
  {-# INLINE margin #-}

instance (a ~ Margin,b ~ a,c ~ b,d ~ (),t ~ t',t' ~ t'',t'' ~ t''') => MarginOverload (Behavior t a -> Behavior t' b -> Behavior t'' c -> CSS t''' d) where
  margin a b c =
    "margin" -:
    (liftA3 (\a' b' c' -> coerce a' <> " " <> coerce b' <> " " <> coerce c') a b c)
  {-# INLINE margin #-}

instance (a ~ Margin,b ~ a,c ~ b,d ~ c,e ~ (),t ~ t',t' ~ t'',t'' ~ t''', t''' ~ t'''') => MarginOverload (Behavior t a -> Behavior t' b -> Behavior t'' c -> Behavior t''' d -> CSS t'''' e) where
  margin a b c d =
    "margin" -:
    ((\a' b' c' d' -> coerce a' <> " " <> coerce b' <> " " <> coerce c' <> " " <>
                coerce d') <$> a <*> b <*> c <*> d)
  {-# INLINE margin #-}

--------------------------------------------------------------------------------
newtype Padding = Padding JSString

instance Length Padding
instance Percentage Padding

class PaddingOverload a where
  padding :: a

instance (a ~ Padding,b ~ (), t ~ t') => PaddingOverload (Behavior t a -> CSS t' b) where
  padding v = "padding" -: fmap coerce v
  {-# INLINE padding #-}

instance (a ~ Padding,b ~ a,c ~ (), t ~ t', t' ~ t'') => PaddingOverload (Behavior t a -> Behavior t' b -> CSS t'' c) where
  padding a b =
    "padding" -:
    (liftA2 (\a' b' -> coerce a' <> " " <> coerce b') a b)
  {-# INLINE padding #-}

instance (a ~ Padding,b ~ a,c ~ b,d ~ (),t ~ t',t' ~ t'',t'' ~ t''') => PaddingOverload (Behavior t a -> Behavior t' b -> Behavior t'' c -> CSS t''' d) where
  padding a b c =
    "padding" -:
    (liftA3 (\a' b' c' -> coerce a' <> " " <> coerce b' <> " " <> coerce c') a b c)
  {-# INLINE padding #-}

instance (a ~ Padding,b ~ a,c ~ b,d ~ c,e ~ (),t ~ t',t' ~ t'',t'' ~ t''', t''' ~ t'''') => PaddingOverload (Behavior t a -> Behavior t' b -> Behavior t'' c -> Behavior t''' d -> CSS t'''' e) where
  padding a b c d =
    "padding" -:
    ((\a' b' c' d' -> coerce a' <> " " <> coerce b' <> " " <> coerce c' <> " " <>
                coerce d') <$> a <*> b <*> c <*> d)
  {-# INLINE padding #-}

------------------------------------text---------------------------------------------
newtype TextTransform = TextTransform JSString

capitalize, uppercase, lowercase :: Behavior t TextTransform
capitalize = pure (coerceStr "capitalize")
uppercase = pure (coerceStr "uppercase")
lowercase = pure (coerceStr "lowercase")
{-# INLINE capitalize #-}
{-# INLINE uppercase #-}
{-# INLINE lowercase #-}

textTransform :: Behavior t TextTransform -> CSS t ()
textTransform v = "text-transform" -: fmap coerce v
{-# INLINE textTransform #-}

--------------------------------------------------------------------------------
newtype Overflow = Overflow JSString

hidden :: Behavior t Overflow
hidden = pure (coerceStr "hidden")
{-# INLINE hidden #-}

overflow :: Behavior t Overflow -> CSS t ()
overflow v = "overflow" -: fmap coerce v
{-# INLINE overflow #-}
