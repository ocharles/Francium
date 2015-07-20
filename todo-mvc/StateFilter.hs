{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module StateFilter where

import Control.FRPNow
import Control.Lens ((.=))
import Data.Monoid ((<>))
import Data.Traversable (for)
import Francium.CSS hiding (Filter, merge)
import Francium.HTML
import Francium.Hooks
import IdiomExp
import Prelude hiding (div, span)
import ToDoItem (Status(..))
import GHCJS.Foreign

--------------------------------------------------------------------------------
data StateFilter =
  StateFilter {stateFilterF :: Behavior (Status -> Bool)
              ,renderStateFilter :: HTML Behavior ()}

newStateFilter :: Now StateFilter
newStateFilter =
  mdo stateFilters <-
        for [minBound .. maxBound]
            (\filter_ ->
               do let active =
                        fmap (filter_ ==) currentState
                  newFilterSelector
                    FilterSelectorConfig {filterSelectorBy = filter_
                                         ,filterSelectorIsActive = active})
      currentState <-
        sample (fromChanges initialState
                            (foldl merge mempty (fmap filterClicked stateFilters)))
      return StateFilter {renderStateFilter =
                            into container
                                 (mconcat (fmap (into selectorCell)
                                                (fmap renderFilterSelector stateFilters)))
                         ,stateFilterF =
                            fmap (\case
                                    All ->
                                      const True
                                    Active -> (== Incomplete)
                                    Completed -> (== Complete))
                                 currentState}
  where container =
          ul_ (style .=
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
data FilterBy
  = All
  | Active
  | Completed
  deriving (Bounded,Enum,Eq,Ord,Show)

data FilterSelectorConfig =
  FilterSelectorConfig {filterSelectorBy :: FilterBy
                       ,filterSelectorIsActive :: Behavior Bool}

data FilterSelector =
  FilterSelector {filterClicked :: EvStream FilterBy
                 ,renderFilterSelector :: HTML Behavior ()}

data FilterSelectorState
  = Selected
  | Hover
  | NoSelection

newFilterSelector :: FilterSelectorConfig -> Now FilterSelector
newFilterSelector FilterSelectorConfig{..} =
  do (hoverHook,isHovering) <- newHoverHook
     (clickHook,clicked) <- newClickHook
     let selectionState =
           $(i [|case $(i [|(filterSelectorIsActive,isHovering)|]) of
                   (True,_) -> Selected
                   (_,True) -> Hover
                   (_,False) -> NoSelection|])
     return FilterSelector {filterClicked = filterSelectorBy <$ clicked
                           ,renderFilterSelector =
                              modifyElementB
                                (fmap renderStateSelector selectionState)
                                (a_ (applyHooks (hoverHook <> clickHook))
                                    (text (toJSString (show filterSelectorBy))))}
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
