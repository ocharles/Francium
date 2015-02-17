{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module StateFilter (StateFilter(..), stateFilterF) where

import Anchor
import Clay.Common
import Clay.Size
import Clay.Color
import Clay.Display
import Clay.Geometry
import Clay.Border
import Clay.Text
import Clay.Font
import Clay.List
import Control.Lens ((.=))
import Control.Monad.Trans.State.Strict (execState)
import Data.Traversable (for)
import Francium
import Francium.Component
import Francium.HTML
import HoverObserver
import Prelude hiding (div, span)
import Reactive.Banana
import ToDoItem (Status(..))

--------------------------------------------------------------------------------
data StateFilter t =
  StateFilter

instance Component StateFilter where
  data Output behavior event
       StateFilter = StateFilterOutputs{stateFilterF ::
                                        behavior (Status -> Bool)}
  construct StateFilter =
    mdo stateFilters <-
          for [minBound .. maxBound]
              (\filter_ ->
                 do let active =
                          fmap (filter_ ==) currentState
                    construct (FilterSelector {filterType = filter_
                                              ,isActive = active}))
        let currentState =
              stepper initialState (unions (fmap (filterClicked . outputs) stateFilters))
        return Instantiation {render =
                                fmap (into container .
                                      fmap (into selectorCell . pure))
                                     (traverse render stateFilters)
                             ,outputs =
                                StateFilterOutputs
                                  (fmap (\case
                                           All ->
                                             const True
                                           Active -> (== Incomplete)
                                           Completed -> (== Complete))
                                        currentState)}
    where container =
            with ul
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
                 []
          selectorCell =
            with li (style .= display inline) []
          initialState = All

--------------------------------------------------------------------------------
data Filter
  = All
  | Active
  | Completed
  deriving (Bounded,Enum,Eq,Ord,Show)

data FilterSelector t =
  FilterSelector {filterType :: Filter
                 ,isActive :: Behavior t Bool}

data FilterSelectorState
  = Selected
  | Hover
  | NoSelection

instance Component FilterSelector where
  data Output behavior event
       FilterSelector = FilterOutput{filterClicked :: event Filter}
  construct fc =
    do baseAnchor <-
         construct (HoverObserver (Anchor [text (show (filterType fc))]))
       let selectionState =
             liftA2 (\isHovering isSelected ->
                       if isSelected
                          then Selected
                          else if isHovering
                                  then Hover
                                  else NoSelection)
                    (isHovered (outputs baseAnchor))
                    (isActive fc)
       return Instantiation {outputs =
                               FilterOutput {filterClicked =
                                               filterType fc <$
                                               clicked (passThrough (outputs baseAnchor))}
                            ,render =
                               liftA2 renderStateSelector selectionState (render baseAnchor)}
    where renderStateSelector selectionState =
            execState (style .=
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
                               Hover ->
                                 rgba 175 47 47 26
                               Selected ->
                                 rgba 175 47 47 51))
