{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module HoverObserver where

import Control.Monad.State 
import Francium
import Francium.HTML hiding (a)

data Hook = Hook { applyHook :: forall m. MonadState HTML m => m () }

instance Monoid Hook where
  mempty = Hook (return ())
  mappend (Hook a) (Hook y) = Hook (a >> y)

applyHooks :: Hook -> HTML -> HTML
applyHooks = execState . applyHook

newHoverObserver :: Frameworks t => Moment t (Hook, Behavior t Bool)
newHoverObserver =
  do mouseOver <- newDOMEvent
     mouseOut <- newDOMEvent
     let mouseHovering =
           accumB False
                  ((const True <$
                    domEvent mouseOver) `union`
                   (const False <$
                    domEvent mouseOut))
         hook = Hook
           (do onMouseOver mouseOver
               onMouseOut mouseOut)
     return (hook,mouseHovering)
