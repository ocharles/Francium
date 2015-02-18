{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module OpenItemCount where

import Control.Lens ((.=))
import Francium
import Francium.Component
import Clay.Background
import Clay.Font
import Clay.Display
import Francium.HTML
import Clay.Text
import Prelude hiding (span)
import ToDoItem (Status(..))

data OpenItemCount t =
  OpenItemCount {items :: Behavior t [Status]}

instance Component OpenItemCount where
  data Output behavior event OpenItemCount = OpenItemCountOutput
  construct oic =
    do let openItemCount =
             fmap (length .
                   filter (== Incomplete))
                  (items oic)
       return Instantiation {outputs = OpenItemCountOutput
                            ,render =
                               fmap (\n ->
                                       with span
                                            (style .=
                                             (do textAlign (alignSide sideLeft)
                                                 float floatLeft))
                                            [with strong
                                                  (style .=
                                                   fontWeight (weight 300))
                                                  [text (show n)]
                                            ," "
                                            ,if n == 1
                                                then "item"
                                                else "items"
                                            ," left"])
                                    openItemCount}
