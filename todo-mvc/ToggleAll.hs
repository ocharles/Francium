{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module ToggleAll where

import Data.Bool
import Control.Lens (at, (?=))
import Control.Monad (when)
import Francium
import Francium.Component
import Francium.HTML
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
       toggle <- newDOMEvent
       return Instantiation {outputs =
                               ToggleAllOut
                                 (fmap (bool Complete Incomplete)
                                       (allComplete <@ domEvent toggle))
                            ,render =
                               fmap (\c ->
                                       with input
                                            (do attrs .
                                                  at "type" ?=
                                                  "checkbox"
                                                attrs .
                                                  at "style" ?=
                                                  "outline-style: none; border-style: none; text-align: center; height: 34px; width: 60px; left: -12px; top: -55px; position: absolute; background-image: none;"
                                                onClick toggle
                                                when c
                                                     (attrs .
                                                      at "checked" ?=
                                                      "checked"))
                                            [])
                                    allComplete}
