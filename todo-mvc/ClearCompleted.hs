{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ClearCompleted where

import Control.Lens ((.=))
import Francium
import Francium.CSS
import Francium.Component
import Francium.HTML
import Francium.Hooks

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
