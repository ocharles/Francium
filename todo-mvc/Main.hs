{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

import Clay ((-:))
import Clay.Background
import Clay.Border
import Clay.Box
import Clay.Color
import Clay.Common as Css
import Clay.Display
import Clay.Font
import Clay.Geometry
import Clay.Size
import Clay.Text
import ClearCompleted
import Control.Lens ((?=), (.=), at)
import Control.Monad
import Francium
import Francium.Component
import Francium.HTML hiding (em, map)
import NewItemAdder
import Prelude hiding (div, span)
import StateFilter
import ToDoItem
import ToDoList
import ToggleAll

main :: IO ()
main = react app

-- | 'app' defines our TodoMVC clone's top-level definition. To Francium, web
-- applications are simply time-varying HTML documents, and we can see this from
-- the type of 'app' - after building an event network in the 'Moment' monad, it
-- returns a single 'Behavior' containing 'HTML' values.
--
-- Inside our 'app' function, we simply build all the necessary components for
-- the TodoMVC application, and wire them together appropriately.
app :: Frameworks t => Moment t (Behavior t HTML)
app =
  mdo
      -- The newItemAdder component allows users to append new items to their
      -- to-do list.
      newItemAdder <- construct NewItemAdder
      -- The state filter component allows users to show all to-do items, or
      -- only complete/incomplete items.
      stateFilter <- construct StateFilter
      -- The clearCompleted component provides a button that will remove all
      -- completed to-do items, leaving just incomplete items.
      clearCompleted <- construct ClearCompleted
      -- The toggle all component is a checkbox that updates the status of all
      -- to-do items. If any are incomplete, then toggling it will mark all
      -- items as complete. If all items are complete, toggling it will mark all
      -- items as incomplete.
      --
      -- Here we see our first component that requires external inputs.
      -- Specifically, the ToggleAll component needs to know the status of items
      -- in the to-do list. We simply this data by proxying through the contents
      -- of the ToDoList component, which we construct next. (Note that we can
      -- refer to declarations created later by using @mdo@ syntax).
      toggleAll <-
        construct (ToggleAll {items =
                                allItems (outputs toDoList)})
      -- Finally, we construct the toDoList component, which renders all known
      -- to-do items, along with managing the state and persistance of the to-do
      -- list.
      --
      -- This component has many external inputs:
      toDoList <-
        construct (ToDoList {ToDoList.addItem =
                               -- An event to add a new item to the to-do list.
                               NewItemAdder.addItem (outputs newItemAdder)
                            ,ToDoList.clearCompleted =
                               -- An event to clear all completed to-do items by
                               -- removing them from the list.
                               ClearCompleted.clearCompleted (outputs clearCompleted)
                            ,statusFilter =
                               -- A function (that changes over time) indicating
                               -- which to-do items the user wishes to view.
                               StateFilter.stateFilterF (outputs stateFilter)
                            ,setStatuses =
                               -- An event that updates the status of all to-do
                               -- items.
                               toggleUpdate (outputs toggleAll)})
      let openItemCount =
            fmap (length .
                  filter (== Incomplete))
                 (allItems (outputs toDoList))
      return (fmap appView
                   (TodoApp <$> render newItemAdder <*> render toDoList <*>
                    fmap (not . null)
                         (allItems (outputs toDoList)) <*>
                    render stateFilter <*>
                    openItemCount <*>
                    render clearCompleted <*>
                    render toggleAll))

mainContainer :: HTML
mainContainer =
  with div
       (style .=
        do sym padding (px 0)
           sym2 margin (px 0) auto
           fontWeight (weight 300)
           maxWidth (px 550)
           minWidth (px 230)
           color (rgb 77 77 77)
           lineHeight (em 1.4)
           fontFamily ["Helvetica Neue","Helvetica","Arial"]
                      [sansSerif]
           fontSize (px 14)
           "font-stretch" -: "normal"
           fontVariant normal
           fontStyle normal
           backgroundColor (rgb 245 245 245))
       []

pageTitle :: HTML
pageTitle =
  with h1
       (style .=
        do color (rgba 175 47 47 39)
           textAlign (alignSide sideCenter)
           fontWeight (weight 100)
           fontSize (px 100)
           width (pct 100)
           top (px (-155))
           position absolute)
       ["todos"]

toDoSummary :: Int -> HTML -> HTML -> HTML
toDoSummary n stateFilter clearCompletedButton =
  with footer
       (style .=
        do borderTopColor (rgb 230 230 230)
           borderTopStyle solid
           borderTopWidth (px 1)
           textAlign (alignSide sideCenter)
           height (px 20)
           sym2 padding
                (px 10)
                (px 15)
           color (rgb 119 119 119))
       [with div
             (style .=
              do position absolute
                 right (px 0)
                 bottom (px 0)
                 left (px 0)
                 height (px 50)
                 --"box-shadow: 0 1px 1px rgba(0, 0, 0, 0.2), 0 8px 0 -3px #f6f6f6, 0 9px 1px -3px rgba(0, 0, 0, 0.2), 0 16px 0 -6px #f6f6f6, 0 17px 2px -6px rgba(0, 0, 0, 0.2);")
                 overflow hidden)
             []
       ,with span
             (style .=
              (do textAlign (alignSide sideLeft)
                  float floatLeft))
             [with strong
                   (style .=
                    fontWeight (weight 300))
                   [text (show n)]
             ," "
             ,if n == 1
                 then "item"
                 else "items"
             ," left"]
       ,stateFilter
       ,clearCompletedButton
       ,with button
             (style .=
              do verticalAlign baseline
                 fontSize (pct 100)
                 borderWidth (px 0)
                 sym padding (px 0)
                 sym margin (px 0)
                 outlineStyle none
                 position relative
                 visibility hidden
                 cursor pointer
                 textDecorationLine none
                 lineHeight (px 20)
                 float floatRight
                 backgroundImage none)
             []]

pageFooter :: HTML
pageFooter =
  with footer
       (style .=
        do textAlign (alignSide sideCenter)
           textShadow (px 0)
                      (px 1)
                      (px 0)
                      (rgba 255 255 255 127)
           fontSize (px 10)
           color (rgb 191 191 191)
           sym3 margin
                (px 65)
                auto
                (px 0))
       [with p
             (style .=
              lineHeight (1 :: Size Rel))
             ["Double-click to edit a todo"]
       ,with p
             (style .=
              lineHeight (1 :: Size Rel))
             ["Template by "
             ,with a
                   (do style .=
                         do fontWeight (weight 400)
                            textDecorationLine none
                       attrs .
                         at "href" ?=
                         "http://sindresorhus.com")
                   ["Sindre Sorhus"]]
       ,with p
             (style .=
              lineHeight (1 :: Size Rel))
             ["Created by "
             ,with a
                   (do style .=
                         do fontWeight (weight 400)
                            textDecorationLine none
                       attrs .
                         at "href" ?=
                         "http://todomvc.com")
                   ["you"]]
       ,with p
             (style .=
              lineHeight (1 :: Size Rel))
             ["Part of "
             ,with a
                   (do style .=
                         do fontWeight (weight 400)
                            textDecorationLine none
                       attrs .
                         at "href" ?=
                         "http://todomvc.com")
                   ["TodoMVC"]]]

data TodoApp =
  TodoApp {taAddANewItem :: HTML
          ,taToDoList :: HTML
          ,taHasItems :: Bool
          ,taStateFilter :: HTML
          ,taOpenItemCount :: Int
          ,taClearCompleted :: HTML
          ,taToggleAll :: HTML}

appView :: TodoApp -> HTML
appView components =
  into mainContainer
       [with section
             (style .=
              do boxShadows [(px 0,px 2,px 4,rgba 0 0 0 51)]
                 position relative
                 margin (px 130)
                        (px 0)
                        (px 40)
                        (px 0)
                 backgroundColor (rgb 255 255 255))
             (into header [pageTitle,taAddANewItem components] :
              case taHasItems components of
                False -> []
                True ->
                  [with section
                        (style .=
                         do borderTop solid
                                      (px 1)
                                      (rgb 230 230 230)
                            zIndex 2
                            position relative)
                        [taToggleAll components
                        ,with label
                              (do style .= display none
                                  attrs .
                                    at "for" ?=
                                    "toggle-all")
                              ["Mark all as complete"]
                        ,taToDoList components]
                  ,toDoSummary (taOpenItemCount components)
                               (taStateFilter components)
                               (taClearCompleted components)])
       ,pageFooter]
