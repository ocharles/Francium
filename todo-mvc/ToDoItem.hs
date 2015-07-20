{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module ToDoItem where

import Control.FRPNow
import Control.Lens ((?=), (.=), (<~))
import Control.Monad.Trans.Class
import Control.Monad (void)
import Data.Bool (bool)
import Data.Monoid ((<>))
import Francium
import Francium.CSS hiding (merge)
import Francium.HTML
import Francium.Hooks
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
data ToDoItemConfig =
  ToDoItemConfig {toDoItemInitialContent :: JSString
                 ,toDoItemStatus :: Behavior Status}

defaultToDoItemConfig :: ToDoItemConfig
defaultToDoItemConfig =
  ToDoItemConfig {toDoItemInitialContent = ""
                 ,toDoItemStatus = pure Incomplete}

data ToDoItem =
  ToDoItem {toDoItemStatusChanges :: EvStream Status
           ,toDoItemDestroyed :: Event ()
           ,toDoItemValue :: Behavior JSString
           ,renderToDoItem :: HTML Behavior ()}

newToDoItem :: ToDoItemConfig -> Now ToDoItem
newToDoItem ToDoItemConfig{..} =
  mdo (hookHoverContainer,isHoveringRow) <- newHoverHook
      (hookKeyPresses,keyPressed) <- newKeyPressHook
      (hookFocus,lostFocus) <- newBlurHook
      (clickHook,click) <- newClickHook
      (hookEditFieldRender,editFieldRendered) <- newRenderHook
      callIOStream (nextTick . elementFocus)
                   editFieldRendered
      textInput <-
        newTextInput (defaultTextInputConfig {textInputInitialText = toDoItemInitialContent})
      destroyButton <-
        newButton defaultButtonConfig {buttonLabel = "\215"}
      statusCheckbox <-
        newToDoCheckbox
          (defaultToDoCheckboxConfig {toDoCheckboxChecked =
                                        fmap (\case
                                                Incomplete -> False
                                                Complete -> True)
                                             toDoItemStatus})
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
          itemValue = textInputValue textInput
      selfDestruct <-
        sample (next (merge (buttonClicks destroyButton)
                            (during switchToViewing (fmap isEmptyString itemValue))))
      contentFromNow <-
        sample (fromChanges toDoItemInitialContent
                            (snapshots itemValue switchToViewing))
      let self =
            ToDoItem {renderToDoItem =
                        embed (itemRenderer clickHook <$>
                               observeHTML (renderButton destroyButton) <*>
                               observeHTML (renderToDoCheckbox statusCheckbox) <*>
                               pure (div_ (applyHooks hookHoverContainer) mempty) <*>
                               showDestroy <*>
                               state <*>
                               observeHTML
                                 (modifyElement
                                    (applyHooks
                                       (hookKeyPresses <> hookFocus <>
                                        hookEditFieldRender))
                                    (renderTextInput textInput)) <*>
                               itemValue <*>
                               toDoItemStatus)
                     ,toDoItemStatusChanges =
                        fmap (\b ->
                                bool Incomplete Complete b)
                             (toDoCheckboxChanges statusCheckbox)
                     ,toDoItemDestroyed = selfDestruct
                     ,toDoItemValue = contentFromNow}
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
data ToDoCheckboxConfig =
  ToDoCheckboxConfig {toDoCheckboxChecked :: Behavior Bool}

defaultToDoCheckboxConfig :: ToDoCheckboxConfig
defaultToDoCheckboxConfig =
  ToDoCheckboxConfig {toDoCheckboxChecked =
                        pure False}

data ToDoCheckbox =
  ToDoCheckbox {toDoCheckboxChanges :: EvStream Bool
               ,renderToDoCheckbox :: HTML Behavior ()}

newToDoCheckbox :: ToDoCheckboxConfig -> Now ToDoCheckbox
newToDoCheckbox ToDoCheckboxConfig{..} =
  do (clickHook,click) <- newClickHook
     toggled_ <-
       sample (scanlEv (\a f -> f a)
                       False
                       (merge (not <$ click)
                              (const <$> toChanges toDoCheckboxChecked)))
     isChecked <-
       sample (fromChanges False toggled_)
     return ToDoCheckbox {toDoCheckboxChanges = toggled_
                         ,renderToDoCheckbox =
                            input_ (do applyHooks clickHook
                                       checked <~ lift isChecked
                                       type_ ?= "checkbox")
                                   mempty}

--------------------------------------------------------------------------------
data ButtonConfig =
  ButtonConfig {buttonLabel :: HTML Behavior ()}

defaultButtonConfig :: ButtonConfig
defaultButtonConfig =
  ButtonConfig {buttonLabel = ""}

data Button =
  Button {buttonClicks :: EvStream ()
         ,renderButton :: HTML Behavior ()}

newButton :: ButtonConfig -> Now Button
newButton ButtonConfig{..} =
  do (clickHook,click) <- newClickHook
     return Button {buttonClicks = click
                   ,renderButton =
                      button_ (applyHooks clickHook) buttonLabel}
