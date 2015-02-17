{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ClearCompleted where

import Clay.Common
import Clay.Display
import Clay.Font
import Clay.Size
import Clay.Text
import Control.Lens ((.=))
import Francium
import Francium.Component
import Francium.HTML hiding (b, html)
import HoverObserver
import PureComponent

data ClearCompleted t =
  ClearCompleted

instance Component ClearCompleted where
  data Output behavior event
       ClearCompleted = ClearCompletedOutput{clearCompleted :: event ()}
  construct _ =
    do b <-
         construct (HoverObserver (PureComponent button))
       click <- newDOMEvent
       return Instantiation {outputs =
                               ClearCompletedOutput {clearCompleted = domEvent click}
                            ,render =
                               liftA2 (\h html ->
                                         with html
                                              (do style .=
                                                    (do float floatRight
                                                        position relative
                                                        lineHeight (px 20)
                                                        textDecoration none
                                                        cursor pointer)
                                                  onClick click)
                                              ["Clear Completed"])
                                      (isHovered (outputs b))
                                      (render b)}
