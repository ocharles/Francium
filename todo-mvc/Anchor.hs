{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Anchor where

import Control.Lens ((?=), at)
import Francium
import Francium.Component
import Francium.HTML
import Francium.Hooks
import VirtualDom

data Anchor t =
  Anchor {content :: [HTML]}

instance Component Anchor where
  data Output behavior event Anchor = AnchorOutput{clicked ::
                                                 event ()}
  construct anchor =
    do (clickHook,c) <- newClickHook
       return Instantiation {outputs =
                               AnchorOutput {clicked = c}
                            ,render =
                               pure (with (applyHooks clickHook a_)
                                          (attributes .
                                           at "href" ?=
                                           "#/")
                                          (content anchor))}
