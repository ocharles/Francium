{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Anchor where

import Control.Lens ((?=), at)
import Francium
import Francium.Component
import Francium.HTML

data Anchor = Anchor { content :: [HTML]}

instance Component Anchor where
  data Output behavior event Anchor = AnchorOutput{clicked ::
                                                 event ()}
  construct anchor =
    do c <- newDOMEvent
       return Instantiation {outputs =
                               AnchorOutput {clicked = domEvent c}
                            ,render =
                               pure (with a
                                          (do onClick c
                                              attrs .
                                                at "href" ?=
                                                "#/")
                                          (content anchor))}
