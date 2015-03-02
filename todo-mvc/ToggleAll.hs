{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ToggleAll where

import Control.Lens ((?=), (.=))
import Data.Bool
import Francium
import Francium.CSS hiding (all)
import Francium.Component
import Francium.HTML
import Francium.Hooks
import ToDoItem (Status(..))

data ToggleAll t =
  ToggleAll {items :: Behavior t [Status]}

instance Component ToggleAll where
  data Output behavior event ToggleAll = ToggleAllOut{toggleUpdate ::
                                                    event Status}
  construct tAll =
    do let allComplete =
             fmap (all (== Complete))
                  (items tAll)
       (clickHook,toggle) <- newClickHook
       return Instantiation {outputs =
                               ToggleAllOut
                                 (fmap (bool Complete Incomplete)
                                       (allComplete <@ toggle))
                            ,render =
                               fmap (\c ->
                                       with (applyHooks clickHook input_)
                                            (do type_ ?= "checkbox"
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
                                            [])
                                    allComplete}
