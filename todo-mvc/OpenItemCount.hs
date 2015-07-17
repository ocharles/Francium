{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module OpenItemCount where

import Control.Lens ((.=))
import Francium.CSS hiding (filter)
import Francium.Component
import Francium.HTML
import Prelude hiding (span)
import ToDoItem (Status(..))
import Control.FRPNow
import GHCJS.Foreign

data OpenItemCount =
  OpenItemCount {items :: Behavior [Status]}

instance Component OpenItemCount where
  data Output OpenItemCount = OpenItemCountOutput
  construct oic =
    do let openItemCount =
             fmap (length . filter (== Incomplete))
                  (items oic)
       return Instantiation {outputs = OpenItemCountOutput
                            ,render =
                               embed (fmap (\n ->
                                              span_ (style .=
                                                     (do textAlign (alignSide sideLeft)
                                                         float floatLeft))
                                                    (mconcat [strong_ (style .=
                                                                       fontWeight (weight 300))
                                                                      (text (toJSString (show n)))
                                                             ," "
                                                             ,if n == 1
                                                                 then "item"
                                                                 else "items"
                                                             ," left"]))
                                           openItemCount)}
