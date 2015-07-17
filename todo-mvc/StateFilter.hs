{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module StateFilter (StateFilter(..), stateFilterF) where

import Control.FRPNow
import Control.Lens ((.=))
import Data.Monoid ((<>))
import Data.Traversable (for)
import Francium.CSS hiding (Filter, merge)
import Francium.Component
import Francium.HTML
import Francium.Hooks
import IdiomExp
import Prelude hiding (div, span)
import ToDoItem (Status(..))
import GHCJS.Foreign

--------------------------------------------------------------------------------
data StateFilter =
  StateFilter

instance Component StateFilter where
  data Output StateFilter = StateFilterOutputs{stateFilterF ::
                                             Behavior (Status -> Bool)}
  construct StateFilter =
    mdo stateFilters <-
          for [minBound .. maxBound]
              (\filter_ ->
                 do let active =
                          fmap (filter_ ==) currentState
                    construct (FilterSelector {filterType = filter_
                                              ,isActive = active}))
        currentState <-
          sample (fromChanges
                    initialState
                    (foldl merge mempty (fmap (filterClicked . outputs) stateFilters)))
        return Instantiation {render =
                                into container
                                     (mconcat (fmap (into selectorCell)
                                                    (fmap render stateFilters)))
                             ,outputs =
                                StateFilterOutputs
                                  (fmap (\case
                                           All ->
                                             const True
                                           Active -> (== Incomplete)
                                           Completed -> (== Complete))
                                        currentState)}
    where container =
            ul_
                 (style .=
                  do left (px 0)
                     right (px 0)
                     position absolute
                     listStyleType none
                     padding (px 0)
                             (px 0)
                             (px 0)
                             (px 0)
                     margin (px 0)
                            (px 0)
                            (px 0)
                            (px 0))
                 mempty
          selectorCell =
            li_ (style .= display inline) mempty
          initialState = All

--------------------------------------------------------------------------------
data Filter
  = All
  | Active
  | Completed
  deriving (Bounded,Enum,Eq,Ord,Show)

data FilterSelector =
  FilterSelector {filterType :: Filter
                 ,isActive :: Behavior Bool}

data FilterSelectorState
  = Selected
  | Hover
  | NoSelection

instance Component FilterSelector where
  data Output FilterSelector = FilterOutput{filterClicked ::
                                          EvStream Filter}
  construct fc =
    do (hoverHook,isHovering) <- newHoverHook
       (clickHook,clicked) <- newClickHook
       let selectionState =
             $(i [|case $(i [|(isActive fc,isHovering)|]) of
                     (True,_) -> Selected
                     (_,True) -> Hover
                     (_,False) -> NoSelection|])
       return Instantiation {outputs =
                               FilterOutput {filterClicked = filterType fc <$
                                                             clicked}
                            ,render =
                               modifyElementB
                                 (fmap renderStateSelector selectionState)
                                 (a_ (applyHooks (hoverHook <> clickHook))
                                     (text (toJSString (show (filterType fc)))))}
    where renderStateSelector selectionState =
            style .=
            do borderWidth (px 1)
               borderStyle solid
               textDecorationLine none
               padding (px 3)
                       (px 7)
                       (px 3)
                       (px 7)
               margin (px 3)
                      (px 3)
                      (px 3)
                      (px 3)
               color inherit
               borderColor
                 (case selectionState of
                    NoSelection -> transparent
                    Hover -> rgba 175 47 47 26
                    Selected -> rgba 175 47 47 51)
