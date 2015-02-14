{-# LANGUAGE TypeFamilies #-}

module HoverObserver where

import Control.Monad.Trans.State.Strict
import Francium
import Francium.Component
import Francium.HTML hiding (a)

data HoverObserver a =
  HoverObserver a

instance Component a => Component (HoverObserver a) where
  data Output behavior event
       (HoverObserver a) = HoverObserverOutput{passThrough ::
                                               Output behavior event a,
                                               isHovered :: behavior Bool}
  construct (HoverObserver a) =
    do inner <- construct a
       mouseOver <- newDOMEvent
       mouseOut <- newDOMEvent
       let mouseHovering =
             accumB False
                    ((const True <$
                      domEvent mouseOver) `union`
                     (const False <$
                      domEvent mouseOut))
       return Instantiation {outputs =
                               HoverObserverOutput {passThrough = outputs inner
                                                   ,isHovered = mouseHovering}
                            ,render =
                               fmap (execState (do onMouseOver mouseOver
                                                   onMouseOut mouseOut))
                                    (render inner)}
