{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ToDoItem where

import IdiomExp
import Data.Monoid ((<>))
import Clay ((-:))
import Clay.Time
import Clay.Background
import Clay.Border
import Clay.Box
import Clay.Color
import Clay.Transition
import Clay.Common as Css
import Clay.Display
import Clay.Text
import Clay.Font
import Clay.Geometry
import Clay.Size
import Control.Lens ((?=), (.=), at)
import Control.Monad (void)
import Control.Monad.Trans.State.Strict (execState)
import Data.Bool (bool)
import Francium
import Francium.Component
import Francium.HTML hiding (b, em, i, pre)
import GHC.Generics
import GHCJS.Foreign
import GHCJS.Types
import HoverObserver
import KeyPressObserver
import Prelude hiding (div, map, span)
import Reactive.Banana
import TextInput
import TrackFocus

data Status = Complete | Incomplete
  deriving (Bounded, Enum, Eq, Ord, Show)

negateStatus :: Status -> Status
negateStatus =
  \case
    Incomplete -> Complete
    Complete -> Incomplete

deriving instance Generic (Output b e ToDoItem)
instance TrimOutput ToDoItem

data State = Viewing | Editing deriving (Eq)

--------------------------------------------------------------------------------
data ToDoItem t =
  ToDoItem {initialContent :: JSString
           ,setStatus :: Event t Status}

instance Component ToDoItem where
  data Output behavior event ToDoItem = ToDoItemOutput{status ::
                                                     behavior Status,
                                                     destroy :: event (),
                                                     steppedContent :: behavior JSString}
  construct toDoItem =
    do (hookHoverContainer,isHoveringRow) <- newHoverObserver
       (hookKeyPresses,keyPressed) <- newKeyPressObserver
       (hookFocus,lostFocus) <- newFocusTracker
       textInput <-
         construct (TextInput (initialContent toDoItem) never)
       destroyButton <-
         construct (Button ["\215"])
       statusCheckbox <-
         do let toggle =
                  fmap (\case
                          Incomplete -> False
                          Complete -> True)
                       (setStatus toDoItem)
            construct (ToDoCheckbox toggle)
       click <- newDOMEvent
       let switchToEditing =
             whenE ((Viewing ==) <$> state)
                   (domEvent click)
           switchToViewing =
             whenE (fmap (Editing ==) state)
                   (unions [lostFocus
                           ,void (filterE (`elem` [13,27]) keyPressed)])
           state =
             accumB Viewing
                    (unions [const Editing <$
                             switchToEditing
                            ,const Viewing <$
                             switchToViewing])
           showDestroy =
             $(i [| $(i [| pure Viewing == state |]) && isHoveringRow |])
           itemValue =
             TextInput.value (outputs textInput)
           selfDestruct =
             unions [clicked (outputs destroyButton)
                    ,whenE (fmap isEmptyString itemValue) switchToViewing]
           self =
             Instantiation {render =
                              itemRenderer click <$> render destroyButton <*>
                              render statusCheckbox <*>
                              pure (applyHooks hookHoverContainer div) <*>
                              showDestroy <*>
                              state <*>
                              fmap (applyHooks (hookKeyPresses <> hookFocus))
                                   (render textInput) <*>
                              itemValue <*>
                              (status (outputs self))
                           ,outputs =
                              ToDoItemOutput {status =
                                                accumB Incomplete
                                                       (unions [fmap (\b _ ->
                                                                        bool Incomplete Complete b)
                                                                     (toggled (outputs statusCheckbox))
                                                               ,const <$>
                                                                (setStatus toDoItem)])
                                             ,destroy = selfDestruct
                                             ,steppedContent =
                                                stepper (initialContent toDoItem)
                                                        (itemValue <@
                                                         switchToViewing)}}
       return self
    where itemRenderer labelClick destroyButton statusCheckbox container showDestroy state textInput inputValue currentStatus =
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
                            (case currentStatus of
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
                            (do case currentStatus of
                                  Incomplete -> labelStyle
                                  Complete -> completeLabelStyle
                                onClick labelClick)
                            [text inputValue]
                      ,execState (if showDestroy
                                     then buttonStyle
                                     else hiddenButtonStyle)
                                 destroyButton]
                    Editing ->
                      [execState (do inputStyle
                                     takesFocus)
                                 textInput]
            in into container
                    (execState checkboxStyle statusCheckbox :
                     items)
          inputStyle =
            style .=
            do boxSizing borderBox
               insetBoxShadow inset
                              (px 0)
                              (px (-1))
                              (px 5)
                              (rgba 0 0 0 51)
               borderWidth (px 1)
               borderStyle solid
               borderColor (rgb 153 153 153)
               padding (px 13)
                       (px 17)
                       (px 12)
                       (px 17)
               outlineStyle none
               lineHeight (em 1.4)
               fontSize (px 24)
               width (px 506)
               margin (px 0)
                      (px 0)
                      (px 0)
                      (px 43)
               position relative
          checkboxStyle =
            style .=
            do textAlign (other "centre")
               width (px 40)
               height auto
               position absolute
               top (px 0)
               bottom (px 0)
               margin auto
                      (px 0)
                      auto
                      (px 0)
               borderStyle none
          labelStyle =
            style .=
            do transition "color"
                          (sec 0.4)
                          auto
                          auto
               lineHeight (1.2 :: Size Abs)
               display block
               marginLeft (px 45)
               padding (px 15)
                       (px 60)
                       (px 15)
                       (px 15)
               "word-break" -: "break-word"
               whiteSpace pre
          completeLabelStyle =
            style .=
            do transition "color"
                          (sec 0.4)
                          auto
                          auto
               lineHeight (1.2 :: Size Abs)
               display block
               marginLeft (px 45)
               padding (px 15)
                       (px 60)
                       (px 15)
                       (px 15)
               "word-break" -: "break-word"
               whiteSpace pre
               color (rgb 217 217 217)
               textDecoration lineThrough
          buttonStyle =
            style .=
            do verticalAlign baseline
               fontSize (px 30)
               borderWidth (px 0)
               sym padding (px 0)
               sym3 margin
                    auto
                    (px 0)
                    (px 11)
               outlineStyle none
               transition "color"
                          (sec 0.2)
                          easeOut
                          (sec 0)
               color (rgb 204 154 154)
               height (px 40)
               width (px 40)
               bottom (px 0)
               right (px 10)
               top (px 0)
               position absolute
               display block
               backgroundImage none
               backgroundColor inherit
          hiddenButtonStyle =
            style .=
            do verticalAlign baseline
               fontSize (px 30)
               borderWidth (px 0)
               sym padding (px 0)
               sym3 margin
                    auto
                    (px 0)
                    (px 11)
               outlineStyle none
               transition "color"
                          (sec 0.2)
                          easeOut
                          (sec 0)
               color (rgb 204 154 154)
               height (px 40)
               width (px 40)
               bottom (px 0)
               right (px 10)
               top (px 0)
               position absolute
               display block
               backgroundImage none
               backgroundColor inherit
               display none
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
data ToDoCheckbox t = ToDoCheckbox { reset :: Event t Bool }

instance Component ToDoCheckbox where
  data Output behavior event
       ToDoCheckbox = ToDoCheckboxOutput{toggled :: event Bool}
  construct c =
    do click <- newDOMEvent
       let toggled_ =
             accumE False (unions [not <$ domEvent click,const <$> reset c])
           isChecked = stepper False toggled_
       return Instantiation {outputs =
                               ToDoCheckboxOutput toggled_
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
data Button t = Button [HTML]

instance Component Button where
  data Output behavior event Button = ButtonOutput { clicked :: event () }
  construct (Button buttonLabel) = do
    click <- newDOMEvent
    return Instantiation { outputs = ButtonOutput (domEvent click)
                         , render = pure (with button (onClick click) buttonLabel)}
