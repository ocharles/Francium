{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}

module ToDoItem where

import Francium
import Francium.HTML
import qualified Francium.HTML as HTML
import Control.Lens ((?=), (.=), at)
import Prelude hiding (div, map, span)
import Reactive.Banana
import GHCJS.Foreign
import GHCJS.Types
import Francium.Component

data Status = Complete | Incomplete
  deriving (Bounded, Enum, Eq, Ord)

negateStatus :: Status -> Status
negateStatus =
  \case
    Incomplete -> Complete
    Complete -> Incomplete

instance TrimOutput ToDoItem where
  trimOutput (ToDoItemOutput b c) = ToDoItemOutput <$> trimB b <*> trimE c

data State = Viewing | Editing deriving (Eq)

data ToDoItem = ToDoItem { initialContent :: JSString }

instance Component ToDoItem where
  data Output behavior event ToDoItem = ToDoItemOutput{status ::
                                                     behavior Status,
                                                     destroy :: event ()}
  construct toDoItem =
    do click <- newDOMEvent
       blur <- newDOMEvent
       editInput <- newDOMEvent
       statusCheckboxClicked <- newDOMEvent
       destroy <- newDOMEvent
       let switchToEditing =
             whenE ((Viewing ==) <$> state)
                   (domEvent click)
           state =
             accumB Viewing
                    ((const Editing <$
                      switchToEditing) `union`
                     (const Viewing <$
                      domEvent blur))
           status =
             accumB Incomplete (negateStatus <$ domEvent statusCheckboxClicked)
       return (Instantiation {render =
                                itemRenderer click editInput blur statusCheckboxClicked destroy <$>
                                state <*>
                                stepper (initialContent toDoItem)
                                        (domEvent editInput) <*>
                                status
                             ,outputs =
                                ToDoItemOutput status
                                               (domEvent destroy)})
    where itemRenderer labelClick editInput blur statusCheckboxClicked destroy state inputValue status =
            let svgCheckbox =
                  case state of
                    Viewing ->
                      [with svg
                            (do attrs .
                                  at "width" ?=
                                  "40"
                                attrs .
                                  at "height" ?=
                                  "40"
                                attrs .
                                  at "viewBox" ?=
                                  "-10 -18 100 135")
                            (case status of
                               Complete ->
                                 [checkCircle
                                 ,with path
                                       (do attrs .
                                             at "fill" ?=
                                             "#5dc2af"
                                           attrs .
                                             at "d" ?=
                                             "M72 25L42 71 27 56l-4 4 20 20 34-52z")
                                       []]
                               Incomplete ->
                                 [checkCircle])]
                    Editing -> []
                items =
                  case state of
                    Viewing ->
                      [with label
                            (do case status of
                                  Incomplete -> labelStyle
                                  Complete -> completeLabelStyle
                                onClick labelClick)
                            [text inputValue]
                      ,with button
                            (do buttonStyle
                                onClick destroy)
                            ["\215"]]
                    Editing ->
                      [with input
                            (do inputStyle
                                value ?= inputValue
                                onBlur blur
                                onInput editInput
                                takesFocus)
                            []]
            in into div
                    (with input
                          (do checkboxStyle
                              onClick statusCheckboxClicked
                              attrs .
                                at "type" ?=
                                "checkbox")
                          svgCheckbox :
                     items)
          inputStyle =
            attrs .
            at "style" ?=
            "-webkit-font-smoothing: antialiased; box-sizing: border-box; box-shadow: rgba(0, 0, 0, 0.2) 0px -1px 5px 0px inset; border: 1px solid rgb(153, 153, 153); padding: 13px 17px 12px 17px; outline-style: none; line-height: 1.4em; font-size: 24px; width: 506px; margin: 0 0 0 43px; position: relative;"
          checkboxStyle =
            attrs .
            at "style" ?=
            "text-align: center; width: 40px; height: auto; position: absolute; top: 0; bottom: 0; margin: auto 0; border: none; -webkit-appearance: none; -ms-appearance: none; appearance: none;"
          labelStyle =
            attrs .
            at "style" ?=
            "-webkit-transition: color 0.4s; transition: color 0.4s; line-height: 1.2; display: block; margin-left: 45px; padding: 15px 60px 15px 15px; word-break: break-word; white-space: pre;"
          completeLabelStyle =
            attrs .
            at "style" ?=
            "-webkit-transition: color 0.4s; transition: color 0.4s; line-height: 1.2; display: block; margin-left: 45px; padding: 15px 60px 15px 15px; word-break: break-word; white-space: pre; color: #d9d9d9; text-decoration: line-through;"
          buttonStyle =
            attrs .
            at "style" ?=
            "-webkit-font-smoothing: antialiased; vertical-align: baseline; font-size: 30px; border-width: 0px; padding: 0px; margin: auto 0px 11px; outline-style: none; -webkit-transition: color 0.2s ease-out initial; transition: color 0.2s ease-out initial; color: rgb(204, 154, 154); height: 40px; width: 40px; bottom: 0px; right: 10px; top: 0px; position: absolute; display: block; background-image: none; background-color: inherit;"
          checkCircle =
            with circle
                 (do attrs .
                       at "cx" ?=
                       "50"
                     attrs .
                       at "cy" ?=
                       "50"
                     attrs .
                       at "r" ?=
                       "50"
                     attrs .
                       at "fill" ?=
                       "none"
                     attrs .
                       at "stroke" ?=
                       "#bddad5"
                     attrs .
                       at "stroke-width" ?=
                       "3")
                 []
          svgElement x =
            with (emptyElement x)
                 (namespace .= "http://www.w3.org/2000/svg")
                 []
          svg = svgElement "svg"
          circle = svgElement "circle"
          path = svgElement "path"
