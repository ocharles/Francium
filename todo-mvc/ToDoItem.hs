{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ToDoItem where

import Control.Lens ((?=), (.=), at)
import Control.Monad (void)
import Data.Bool (bool)
import Data.Monoid ((<>))
import Francium
import Francium.CSS
import Francium.Component
import Francium.HTML
import Francium.Hooks
import GHC.Generics
import GHCJS.DOM.Element (elementFocus)
import GHCJS.Foreign
import GHCJS.Types
import IdiomExp
import Prelude hiding (div, map, span)
import TextInput

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
    do (hookHoverContainer,isHoveringRow) <- newHoverHook
       (hookKeyPresses,keyPressed) <- newKeyPressHook
       (hookFocus,lostFocus) <- newBlurHook
       (clickHook,click) <- newClickHook
       (hookEditFieldRender,editFieldRendered) <- newRenderHook
       reactimate (fmap (nextTick . elementFocus) editFieldRendered)
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
       let switchToEditing =
             whenE ((Viewing ==) <$> state) click
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
             $(i [|$(i [|pure Viewing ==
                         state|]) &&
                   isHoveringRow|])
           itemValue =
             TextInput.value (outputs textInput)
           selfDestruct =
             unions [clicked (outputs destroyButton)
                    ,whenE (fmap isEmptyString itemValue) switchToViewing]
           self =
             Instantiation {render =
                              itemRenderer clickHook <$> render destroyButton <*>
                              render statusCheckbox <*>
                              pure (applyHooks hookHoverContainer div_) <*>
                              showDestroy <*>
                              state <*>
                              fmap (applyHooks
                                      (hookKeyPresses <> hookFocus <>
                                       hookEditFieldRender))
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
                            (do width_ ?= "40"
                                height_ ?= "40"
                                attributes .
                                  at "viewBox" ?=
                                  "-10 -18 100 135")
                            (case currentStatus of
                               Complete ->
                                 [checkCircle
                                 ,with path
                                       (do attributes .
                                             at "fill" ?=
                                             "#5dc2af"
                                           attributes .
                                             at "d" ?=
                                             "M72 25L42 71 27 56l-4 4 20 20 34-52z")
                                       []]
                               Incomplete ->
                                 [checkCircle])]
                    Editing -> []
                items =
                  case state of
                    Viewing ->
                      [with (applyHooks labelClick label_)
                            (do case currentStatus of
                                  Incomplete -> labelStyle
                                  Complete -> completeLabelStyle)
                            [text inputValue]
                      ,modifyElement
                         (if showDestroy
                             then buttonStyle
                             else hiddenButtonStyle)
                         destroyButton]
                    Editing ->
                      [modifyElement
                         inputStyle
                         --takesFocus) XXX
                         textInput]
            in into container
                    (modifyElement checkboxStyle statusCheckbox :
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
                 (do attributes .
                       at "cx" ?=
                       "50"
                     attributes .
                       at "cy" ?=
                       "50"
                     attributes .
                       at "r" ?=
                       "50"
                     attributes .
                       at "fill" ?=
                       "none"
                     attributes .
                       at "stroke" ?=
                       "#bddad5"
                     attributes .
                       at "stroke-width" ?=
                       "3")
                 []
          svgElement x =
            with (emptyElement (x :: JSString))
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
    do (clickHook,click) <- newClickHook
       let toggled_ =
             accumE False (unions [not <$ click,const <$> reset c])
           isChecked = stepper False toggled_
       return Instantiation {outputs =
                               ToDoCheckboxOutput toggled_
                            ,render =
                               fmap (\b ->
                                       with (applyHooks clickHook input_)
                                            (do checked .= b
                                                type_ ?= "checkbox")
                                            [])
                                    isChecked}

--------------------------------------------------------------------------------
data Button t = Button [HTML]

instance Component Button where
  data Output behavior event Button = ButtonOutput{clicked ::
                                                 event ()}
  construct (Button buttonLabel) =
    do (clickHook,click) <- newClickHook
       return Instantiation {outputs =
                               ButtonOutput click
                            ,render =
                               pure (into (applyHooks clickHook button_) buttonLabel)}
