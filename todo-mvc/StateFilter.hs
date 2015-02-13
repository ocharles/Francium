{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}

module StateFilter where

import Data.Function (on)
import Data.Traversable (for)
import Francium
import Francium.HTML hiding (map)
import qualified Francium.HTML as HTML
import Control.Lens ((?=), at)
import Prelude hiding (div, span)
import Reactive.Banana
import GHCJS.Foreign
import GHCJS.Types

import ToDoItem (ToDoItem(..), Status(..))

data Filter = All | Active | Completed
  deriving (Bounded, Enum, Eq, Ord, Show)

data StateFilter t =
  StateFilter {sfFilter :: Behavior t (Status -> Bool)
              ,sfView :: Behavior t HTML}

mkStateFilter :: Frameworks t => Moment t (StateFilter t)
mkStateFilter =
  mdo (views,stateChanges) <-
        fmap unzip
             (for [minBound .. maxBound]
                  (\state ->
                     do mouseOver <- newDOMEvent
                        mouseOut <- newDOMEvent
                        changeState <-
                          fmap (state <$) newDOMEvent
                        let mouseHovering =
                              accumB False
                                     ((const True <$
                                       domEvent mouseOver) `union`
                                      (const False <$
                                       domEvent mouseOut))
                            filterSelection =
                              liftA2 (\isHovering isSelected ->
                                        if isSelected
                                           then Selected
                                           else if isHovering
                                                   then Hover
                                                   else NoSelection)
                                     mouseHovering
                                     (fmap (state ==) currentState)
                        return (renderStateSelector <$> filterSelection <*>
                                pure state <*> pure changeState <*>
                                pure mouseOver <*> pure mouseOut
                               ,changeState)))
      let currentState =
            stepper initialState (unions (map domEvent stateChanges))
      return StateFilter {sfView = into container <$> sequenceA views
                         ,sfFilter =
                            (\filter_ ->
                               case filter_ of
                                 All ->
                                   const True
                                 Active -> (== Incomplete)
                                 Completed -> (== Complete)) <$>
                            currentState}
  where container =
          with ul
               (do attrs .
                     at "style" ?=
                     "left: 0px; right: 0px; position: absolute; list-style-type: none; padding: 0px; margin: 0px;")
               []
        initialState = All

data FilterSelection = Selected | Hover | NoSelection
  deriving (Show)

renderStateSelector :: FilterSelection -> Filter -> DOMEvent t () Filter -> DOMEvent t () () -> DOMEvent t () () ->HTML
renderStateSelector selectionState state switch over out =
  with li
       (do attrs .
             at "style" ?=
             "display: inline;")
       [with a
             (do case selectionState of
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
                     "border: 1px solid rgba(175, 47, 47, 0.2); border-radius: 3px; text-decoration-line: none; padding: 3px 7px; margin: 3px; color: inherit"
                 attrs .
                   at "href" ?=
                   "#/"
                 onClick switch
                 onMouseOver over
                 onMouseOut out)
             [text (show state)]]
