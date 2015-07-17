{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ToggleAll where

import Control.Lens ((?=), (.=))
import Data.Bool
import Francium.CSS hiding (all)
import Francium.Component
import Francium.HTML
import Francium.Hooks
import Control.FRPNow
import ToDoItem (Status(..))

data ToggleAll =
  ToggleAll {items :: Behavior [Status]}

instance Component ToggleAll where
  data Output ToggleAll = ToggleAllOut{toggleUpdate ::
                                     EvStream Status}
  construct tAll =
    do let allComplete =
             fmap (all (== Complete))
                  (items tAll)
       (clickHook,toggle) <- newClickHook
       return Instantiation {outputs =
                               ToggleAllOut
                                 (fmap (bool Complete Incomplete)
                                       (snapshots allComplete toggle))
                            ,render =
                               embed (fmap (\c ->
                                              input_ (do applyHooks clickHook
                                                         type_ ?= "checkbox"
                                                         style .=
                                                           do outlineStyle none
                                                              borderStyle none
                                                              textAlign (other "center")
                                                              height (px 34)
                                                              width (px 60)
                                                              left (px (-12))
                                                              top (px (-55))
                                                              position absolute
                                                              backgroundImage none
                                                         checked .= c)
                                                     mempty)
                                           allComplete)}
