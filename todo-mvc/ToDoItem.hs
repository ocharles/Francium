{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecursiveDo #-}

module ToDoItem where

import Control.FRPNow
import Control.Lens ((?=), (.=), at)
import Control.Monad (void)
import Data.Bool (bool)
import Data.Monoid ((<>))
import Francium
import Francium.CSS hiding (merge)
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

data State = Viewing | Editing deriving (Eq)

--------------------------------------------------------------------------------
data ToDoItem =
  ToDoItem {initialContent :: JSString
           ,setStatus :: EvStream Status}

instance Component ToDoItem where
  data Output ToDoItem = ToDoItemOutput{status :: Behavior Status,
                                      destroy :: Event (), steppedContent :: Behavior JSString}
  construct toDoItem =
    mdo (hookHoverContainer,isHoveringRow) <- newHoverHook
        (hookKeyPresses,keyPressed) <- newKeyPressHook
        (hookFocus,lostFocus) <- newBlurHook
        (clickHook,click) <- newClickHook
        (hookEditFieldRender,editFieldRendered) <- newRenderHook
        callIOStream (nextTick . elementFocus)
                     editFieldRendered
        textInput <-
          construct (TextInput (initialContent toDoItem) mempty)
        destroyButton <-
          construct (Button "\215")
        statusCheckbox <-
          do let toggle =
                   fmap (\case
                           Incomplete -> False
                           Complete -> True)
                        (setStatus toDoItem)
             construct (ToDoCheckbox toggle)
        let switchToEditing =
              during click ((Viewing ==) <$> state)
            switchToViewing =
              during (merge lostFocus (void (filterEs (`elem` [13,27]) keyPressed)))
                     (fmap (Editing ==) state)
        state <-
          sample (fromChanges
                    Viewing
                    (merge (Editing <$ switchToEditing)
                           (Viewing <$ switchToViewing)))
        let showDestroy =
              $(i [|$(i [|pure Viewing == state|]) && isHoveringRow|])
            itemValue =
              TextInput.value (outputs textInput)
        selfDestruct <-
          sample (next (merge (clicked (outputs destroyButton))
                              (during switchToViewing (fmap isEmptyString itemValue))))
        statusFromNow <-
          sample (foldEs (\a f -> f a)
                         Incomplete
                         (merge (fmap (\b _ ->
                                         bool Incomplete Complete b)
                                      (toggled (outputs statusCheckbox)))
                                (const <$> (setStatus toDoItem))))
        contentFromNow <-
          sample (fromChanges (initialContent toDoItem)
                              (snapshots itemValue switchToViewing))
        let self =
              Instantiation {render =
                               embed (itemRenderer clickHook <$>
                                      observeHTML (render destroyButton) <*>
                                      observeHTML (render statusCheckbox) <*>
                                      pure (div_ (applyHooks hookHoverContainer) mempty) <*>
                                      showDestroy <*>
                                      state <*>
                                      observeHTML
                                        (modifyElement
                                           (applyHooks
                                              (hookKeyPresses <> hookFocus <>
                                               hookEditFieldRender))
                                           (render textInput)) <*>
                                      itemValue <*>
                                      (status (outputs self)))
                            ,outputs =
                               ToDoItemOutput {status = statusFromNow
                                              ,destroy = selfDestruct
                                              ,steppedContent = contentFromNow}}
        return self
    where itemRenderer labelClick destroyButton statusCheckbox container showDestroy state textInput inputValue currentStatus =
            let
                -- svgCheckbox =
                --   case state of
                --     Viewing ->
                --       [with svg
                --             (do width_ ?= "40"
                --                 height_ ?= "40"
                --                 attributes . at "viewBox" ?= "-10 -18 100 135")
                --             (case currentStatus of
                --                Complete ->
                --                  [checkCircle
                --                  ,with path
                --                        (do attributes . at "fill" ?= "#5dc2af"
                --                            attributes . at "d" ?=
                --                              "M72 25L42 71 27 56l-4 4 20 20 34-52z")
                --                        []]
                --                Incomplete ->
                --                  [checkCircle])]
                --     Editing -> []
                items =
                  case state of
                    Viewing ->
                      [label_ (do applyHooks labelClick
                                  case currentStatus of
                                    Incomplete -> labelStyle
                                    Complete -> completeLabelStyle)
                              (text inputValue)
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
                    (mconcat (modifyElement checkboxStyle statusCheckbox : items))
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
          -- checkCircle =
          --   with circle
          --        (do attributes . at "cx" ?= "50"
          --            attributes . at "cy" ?= "50"
          --            attributes . at "r" ?= "50"
          --            attributes . at "fill" ?= "none"
          --            attributes . at "stroke" ?= "#bddad5"
          --            attributes . at "stroke-width" ?= "3")
          --        []
          -- svgElement x =
          --   term x (namespace .= "http://www.w3.org/2000/svg")
          -- svg = svgElement "svg"
          -- circle = svgElement "circle"
          -- path = svgElement "path"
          isEmptyString x =
            null (fromJSString x :: String)

--------------------------------------------------------------------------------
data ToDoCheckbox =
  ToDoCheckbox {reset :: EvStream Bool}

instance Component ToDoCheckbox where
  data Output ToDoCheckbox = ToDoCheckboxOutput{toggled ::
                                              EvStream Bool}
  construct c =
    do (clickHook,click) <- newClickHook
       toggled_ <-
         sample (scanlEv (\a f -> f a)
                         False
                         (merge (not <$ click)
                                (const <$> reset c)))
       isChecked <-
         sample (fromChanges False toggled_)
       return Instantiation {outputs =
                               ToDoCheckboxOutput toggled_
                            ,render =
                               embed (fmap (\b ->
                                              input_ (do applyHooks clickHook
                                                         checked .= b
                                                         type_ ?= "checkbox")
                                                     mempty)
                                           isChecked)}

--------------------------------------------------------------------------------
data Button =
  Button (HTML Behavior)

instance Component Button where
  data Output Button = ButtonOutput{clicked :: EvStream ()}
  construct (Button buttonLabel) =
    do (clickHook,click) <- newClickHook
       return Instantiation {outputs =
                               ButtonOutput click
                            ,render =
                               button_ (applyHooks clickHook) buttonLabel}
