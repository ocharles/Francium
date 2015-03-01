{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module HoverObserver where

import Data.Monoid
import Francium
import Francium.Hooks

newHoverObserver :: Frameworks t => Moment t (Hook, Behavior t Bool)
newHoverObserver =
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
