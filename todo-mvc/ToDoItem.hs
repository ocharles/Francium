{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module ToDoItem where

import Data.Bool (bool)
import Control.Lens ((?=), (.=), at)
import Control.Monad.Trans.State.Strict (execState)
import Francium
import Francium.Component
import Francium.HTML
import GHC.Generics
import GHCJS.Foreign
import GHCJS.Types
import HoverObserver
import KeyPressObserver
import Prelude hiding (div, map, span)
import PureComponent
import Reactive.Banana
import TextInput
import TrackFocus

data Status = Complete | Incomplete
  deriving (Bounded, Enum, Eq, Ord)

negateStatus :: Status -> Status
negateStatus =
  \case
    Incomplete -> Complete
    Complete -> Incomplete

deriving instance Generic (Output b e ToDoItem)
instance TrimOutput ToDoItem

data State = Viewing | Editing deriving (Eq)

--------------------------------------------------------------------------------
data ToDoItem = ToDoItem { initialContent :: JSString }

instance Component ToDoItem where
  data Output behavior event ToDoItem = ToDoItemOutput{status ::
                                                     behavior Status,
                                                     destroy :: event ()}
  construct toDoItem =
    do container <-
         construct (HoverObserver (PureComponent div))
       textInput <-
         do never' <- trimE never
            construct (TrackFocus (KeyPressObserver (TextInput (initialContent toDoItem) never')))
       destroyButton <-
         construct (Button ["\215"])
       statusCheckbox <- construct ToDoCheckbox
       click <- newDOMEvent
       let switchToEditing =
             whenE ((Viewing ==) <$> state)
                   (domEvent click)
           switchToViewing =
             lostFocus (outputs textInput)
           state =
             accumB Viewing
                    ((const Editing <$
                      switchToEditing) `union`
                     (const Viewing <$
                      switchToViewing))
           status =
             fmap (bool Incomplete Complete)
                  (checked (outputs statusCheckbox))
           showDestroy =
             liftA2 (&&)
                    (fmap (Viewing ==) state)
                    (isHovered (outputs container))
           itemValue =
             TextInput.value (KeyPressObserver.passThrough (TrackFocus.passThrough (outputs textInput)))
           selfDestruct =
             unions [clicked (outputs destroyButton)
                    ,whenE (fmap isEmptyString itemValue) switchToViewing]
       return (Instantiation {render = itemRenderer click <$>
                                       render destroyButton <*>
                                       render statusCheckbox <*>
                                       render container <*> showDestroy <*>
                                       state <*> render textInput <*> itemValue <*>
                                       status
                             ,outputs =
                                ToDoItemOutput status selfDestruct})
    where itemRenderer labelClick destroy statusCheckbox container showDestroy state textInput inputValue status =
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
                      ,execState (if showDestroy
                                     then buttonStyle
                                     else hiddenButtonStyle)
                                 destroy]
                    Editing ->
                      [execState (do inputStyle
                                     takesFocus)
                                 textInput]
            in into container
                    (execState checkboxStyle statusCheckbox :
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
          hiddenButtonStyle =
            attrs .
            at "style" ?=
            "-webkit-font-smoothing: antialiased; vertical-align: baseline; font-size: 30px; border-width: 0px; padding: 0px; margin: auto 0px 11px; outline-style: none; -webkit-transition: color 0.2s ease-out initial; transition: color 0.2s ease-out initial; color: rgb(204, 154, 154); height: 40px; width: 40px; bottom: 0px; right: 10px; top: 0px; position: absolute; display: block; background-image: none; background-color: inherit; display: none;"
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
          isEmptyString x =
            null (fromJSString x :: String)

--------------------------------------------------------------------------------
data ToDoCheckbox = ToDoCheckbox

instance Component ToDoCheckbox where
  data Output behavior event
       ToDoCheckbox = ToDoCheckboxOutput{checked :: behavior Bool}
  construct ToDoCheckbox =
    do click <- newDOMEvent
       let isChecked =
             accumB False (not <$ domEvent click)
       return Instantiation {outputs =
                               ToDoCheckboxOutput isChecked
                            ,render =
                               fmap (\b ->
                                       with input
                                            (do attrs .
                                                  at "checked" .=
                                                  if b
                                                     then Just "checked"
                                                     else Nothing
                                                attrs .
                                                  at "type" ?=
                                                  "checkbox"
                                                onClick click)
                                            [])
                                    isChecked}

--------------------------------------------------------------------------------
data Button = Button [HTML]

instance Component Button where
  data Output behavior event Button = ButtonOutput { clicked :: event () }
  construct (Button content) = do
    click <- newDOMEvent
    return Instantiation { outputs = ButtonOutput (domEvent click)
                         , render = pure (with button (onClick click) content)}
