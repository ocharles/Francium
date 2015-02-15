{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ClearCompleted where

import Control.Lens ((?=), at)
import Control.Monad.Trans.State.Strict (execState)
import Francium
import Francium.Component
import Francium.HTML
import PureComponent
import HoverObserver

data ClearCompleted = ClearCompleted

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
                                              (do attrs .
                                                    at "style" ?=
                                                    "float: right; position: relative; line-height: 20px; text-decoration: none; cursor: pointer; position: relative;"
                                                  onClick click)
                                              ["Clear Completed"])
                                      (isHovered (outputs b))
                                      (render b)}
