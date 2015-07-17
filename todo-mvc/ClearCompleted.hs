{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ClearCompleted where

import Control.FRPNow
import Control.Lens ((.=))
import Francium.CSS
import Francium.Component
import Francium.HTML
import Francium.Hooks

data ClearCompleted =
  ClearCompleted

instance Component ClearCompleted where
  data Output ClearCompleted = ClearCompletedOutput{clearCompleted ::
                                                  EvStream ()}
  construct _ =
    do (hoverHook,isHovering) <- newHoverHook
       (clickHook,click) <- newClickHook
       return Instantiation {outputs =
                               ClearCompletedOutput {clearCompleted = click}
                            ,render =
                               embed (fmap (\h ->
                                              button_ (do applyHooks clickHook
                                                          style .=
                                                            (do float floatRight
                                                                position relative
                                                                lineHeight (px 20)
                                                                textDecoration none
                                                                cursor pointer))
                                                      "Clear Completed")
                                           isHovering)}
