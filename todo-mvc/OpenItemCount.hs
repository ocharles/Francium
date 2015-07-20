{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module OpenItemCount where

import Control.Monad.Trans.Class
import Control.Lens ((.=))
import Francium.CSS hiding (filter)
import Francium.HTML
import Prelude hiding (span)
import ToDoItem (Status(..))
import Control.FRPNow
import GHCJS.Foreign

renderOpenItemCounter :: Behavior [Status] -> HTML Behavior ()
renderOpenItemCounter items =
  span_ (style .=
         (do textAlign (alignSide sideLeft)
             float floatLeft))
        (do n <-
              lift (fmap (length . filter (== Incomplete)) items)
            strong_ (style .= fontWeight (weight 300))
                    (text (toJSString (show n)))
            " "
            if n == 1
               then "item"
               else "items"
            " left")
