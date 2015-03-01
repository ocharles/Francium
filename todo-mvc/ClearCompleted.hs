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
import Francium.HTML
import Francium.Hooks
import VirtualDom.HTML

data ClearCompleted t =
  ClearCompleted

instance Component ClearCompleted where
  data Output behavior event
       ClearCompleted = ClearCompletedOutput{clearCompleted :: event ()}
  construct _ =
    do (hoverHook,isHovering) <- newHoverHook
       (clickHook,click) <- newClickHook
       return Instantiation {outputs =
                               ClearCompletedOutput {clearCompleted = click}
                            ,render =
                               fmap (\h ->
                                       with (applyHooks clickHook button_)
                                            (do style .=
                                                  (do float floatRight
                                                      position relative
                                                      lineHeight (px 20)
                                                      textDecoration none
                                                      cursor pointer))
                                            ["Clear Completed"])
                                    isHovering}
