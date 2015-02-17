{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Francium.CSS (module Clay) where

import Control.Monad.State (MonadState, modify)
import Clay (Color, Size, (!))
import Clay.Color
import Clay.Size
import qualified Clay
import qualified Clay.Stylesheet as Clay
import Data.Text (unpack)
import Data.Text.Lazy (toStrict)
import Data.Monoid

filterInline :: Clay.Css -> Clay.Css
filterInline = mapM_ filterProperty . Clay.runS
  where filterProperty p@Clay.Property{} = Clay.rule p
        filterProperty _ = return ()
