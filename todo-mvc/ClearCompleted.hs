{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ClearCompleted where

import Control.FRPNow
import Control.Lens ((.=))
import Francium.CSS
import Francium.HTML
import Francium.Hooks

data ClearCompleted =
  ClearCompleted {renderClearCompleted :: HTML Behavior ()
                 ,clearCompletedUpdates :: EvStream ()}

newClearCompleted :: Now ClearCompleted
newClearCompleted =
  do (hoverHook,isHovering) <- newHoverHook
     (clickHook,click) <- newClickHook
     return ClearCompleted {clearCompletedUpdates = click
                           ,renderClearCompleted =
                              button_ (do applyHooks clickHook
                                          style .=
                                            (do float floatRight
                                                position relative
                                                lineHeight (px 20)
                                                textDecoration none
                                                cursor pointer))
                                      "Clear Completed"}
