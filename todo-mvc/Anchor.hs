{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Anchor where

import Control.Lens ((?=))
import Francium
import Francium.Component
import Francium.Hooks
import VirtualDom
import VirtualDom.HTML.Attributes

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
                                          (href_ ?= "#/")
                                          (content anchor))}
