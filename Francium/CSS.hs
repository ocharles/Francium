{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Francium.CSS
       (Color, Size, boxShadow, boxShadows, position, module Clay.Color,
        module Clay.Size, Clay.Position(..), Clay.relative, margin, backgroundColor, borderTop, Clay.solid, zIndex, display, Clay.Display, Clay.none)
       where

import Control.Monad.State (MonadState, modify)
import Clay (Color, Size, (!))
import Clay.Color
import Clay.Size
import qualified Clay
import qualified Clay.Stylesheet as Clay
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import Data.Monoid

boxShadow :: MonadState [(String,String)] m
          => Size a -> Size a -> Size a -> Color -> m ()
boxShadow x y w c =
  extractProperty (Clay.boxShadow x y w c)

boxShadows :: MonadState [(String, String)] m => [(Size a, Size a, Size a, Size a, Color)] -> m ()
boxShadows = extractProperty . Clay.prefixed (Clay.browsers <> "box-shadow") . map (\(a, b, c, d, e) -> a ! b ! c ! d ! e)

margin :: MonadState [(String, String)] m => Size a -> Size a -> Size a -> Size a -> m ()
margin a b c d = extractProperty (Clay.margin a b c d)

position :: MonadState [(String, String)] m => Clay.Position -> m ()
position = extractProperty . Clay.position

backgroundColor :: MonadState [(String, String)] m => Color -> m ()
backgroundColor = extractProperty . Clay.backgroundColor

borderTop :: MonadState [(String, String)] m => Clay.Stroke -> Size Clay.Abs -> Color -> m ()
borderTop a b c = extractProperty (Clay.borderTop a b c)

zIndex :: MonadState [(String, String)] m => Integer -> m ()
zIndex = extractProperty . Clay.zIndex

display :: MonadState [(String, String)] m => Clay.Display -> m ()
display = extractProperty . Clay.display

extractProperty :: MonadState [(String, String)] m => Clay.Css -> m ()
extractProperty css =
  modify (++ (findProperty (Clay.runS css)))
  where findProperty [] =
          error ("extractProperty called on something that is not a property: " ++
                 unpack (toStrict (Clay.renderWith Clay.compact [] css)))
        findProperty (Clay.Property (Clay.Key k) (Clay.Value v):_) =
          case (k, v) of
            (Clay.Plain k, Clay.Plain v) -> [(unpack k, unpack v)]
            (Clay.Prefixed ks, Clay.Plain v) -> [(unpack (a <> b), unpack v) | (a,b) <- ks]
        findProperty (_:xs) = findProperty xs
