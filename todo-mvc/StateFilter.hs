{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module StateFilter (StateFilter(..), stateFilterF) where

import Anchor
import Control.Lens ((?=), at)
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
                 (attrs .
                  at "style" ?=
                  "left: 0px; right: 0px; position: absolute; list-style-type: none; padding: 0px; margin: 0px;")
                 []
          selectorCell =
            with li
                 (attrs .
                  at "style" ?=
                  "display: inline;")
                 []
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
            execState (case selectionState of
                         NoSelection ->
                           attrs .
                           at "style" ?=
                           "border: 1px solid transparent; text-decoration-line: none; padding: 3px 7px; margin: 3px; color: inherit"
                         Hover ->
                           attrs .
                           at "style" ?=
                           "border: 1px solid rgba(175, 47, 47, 0.1); border-radius: 3px; text-decoration-line: none; padding: 3px 7px; margin: 3px; color: inherit"
                         Selected ->
                           attrs .
                           at "style" ?=
                           "border: 1px solid rgba(175, 47, 47, 0.2); border-radius: 3px; text-decoration-line: none; padding: 3px 7px; margin: 3px; color: inherit")
