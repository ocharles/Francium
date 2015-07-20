{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module ToggleAll where

import Control.Monad.Trans.Class (lift)
import Control.Lens ((?=), (.=), (<~))
import Data.Bool
import Francium.CSS hiding (all)
import Francium.HTML
import Francium.Hooks
import Control.FRPNow
import ToDoItem (Status(..))

data ToggleAllConfig =
  ToggleAllConfig {toggleAllItems :: Behavior [Status]}

data ToggleAll =
  ToggleAll {renderToggleAll :: HTML Behavior ()
            ,toggleAllUpdates :: EvStream Status}

newToggleAll :: ToggleAllConfig -> Now ToggleAll
newToggleAll ToggleAllConfig{..} =
  do let allComplete =
           fmap (all (== Complete)) toggleAllItems
     (clickHook,toggle) <- newClickHook
     return ToggleAll {toggleAllUpdates =
                         fmap (bool Complete Incomplete)
                              (snapshots allComplete toggle)
                      ,renderToggleAll =
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
                                    checked <~ lift allComplete)
                                mempty}
