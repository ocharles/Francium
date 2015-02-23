{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

import Clay ((-:), Css)
import Clay.Background
import Clay.Border
import Clay.Property
import Clay.Stylesheet
import Data.Monoid
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
import ToDoList
import ToggleAll
import OpenItemCount

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
      -- The openItemCount component simply displays the count of incomplete
      -- todo items.
      --
      -- Here we see our first component that requires external inputs.
      -- Specifically, the openItemCount component needs to know which to-do
      -- items are present. To provide it with this information, we simply
      -- proxy this data through from the contents of the toDoList component,
      -- which we construct next. (Note that we can refer to declarations
      -- created later by using @mdo@ syntax).
      openItemCount <-
        construct OpenItemCount {OpenItemCount.items =
                                   allItems (outputs toDoList)}
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
        construct (ToggleAll {ToggleAll.items =
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
      -- Now that we have constructed all the necessary components, the
      -- remaining step is to lay them out accordingly. To do so, we use
      -- applicative syntax to snapshot the renderings of each child component
      -- at the same point in time, and then lay this out in 'appView'.
      return
        (appView <$> render newItemAdder <*> render toDoList <*>
         fmap (not . null)
              (allItems (outputs toDoList)) <*>
         render stateFilter <*>
         render openItemCount <*>
         render clearCompleted <*>
         render toggleAll)

-- | Now that we have declared our application's event network, the only
-- remaining step is to execute it. To do that, we simply apply 'react' to
-- 'app'. 'react' will watch 'app's HTML 'Behavior', and - whenever it changes -
-- render this into the browsers DOM.
main :: IO ()
main = react app

-- | The 'appView' function simply stiches together all renderings of child
-- components into the main HTML of the document. Francium encourages the use
-- of inline styles, but that requires discipline in making small functions.
-- Here we see that most of the elements that we're adding content to are
-- abstract HTML values.
appView :: HTML -> HTML -> Bool -> HTML -> HTML -> HTML -> HTML -> HTML
appView newItemAdder toDoList hasItems stateFilter openItemCounter clearCompleted toggleAll =
  into mainContainer
       [into rootSection
             (into header [pageTitle,newItemAdder] :
              if hasItems
                 then [into toDoSection [toggleAll,toDoList]
                      ,toDoSummary openItemCounter stateFilter clearCompleted]
                 else [])
       ,pageFooter]

-- | The application itself is rendered into the <body> of the document. Here
-- we style the body accordingly.
mainContainer :: HTML
mainContainer =
  with body
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

rootSection :: HTML
rootSection =
  with section
       (style .=
        do boxShadows
             [(px 0,px 2,px 4,rgba 0 0 0 51),(px 0,px 25,px 50,rgba 0 0 0 25)]
           position relative
           margin (px 130)
                  (px 0)
                  (px 40)
                  (px 0)
           backgroundColor (rgb 255 255 255))
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

toDoSummary :: HTML -> HTML -> HTML -> HTML
toDoSummary openItemCount stateFilter clearCompletedButton =
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
                 boxShadows4
                   [(px 0,px 1,px 1,px 0,rgba 0 0 0 50)
                   ,(px 0,px 8,px 0,px (-3),rgb 246 246 246)
                   ,(px 0,px 9,px 1,px (-3),rgba 0 0 0 50)
                   ,(px 0,px 16,px 0,px (-6),rgb 246 246 246)
                   ,(px 0,px 17,px 2,px (-6),rgba 0 0 0 50)]
                 overflow hidden)
             []
       ,openItemCount
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
              lineHeight (1 :: Size Abs))
             ["Double-click to edit a todo"]
       ,with p
             (style .=
              lineHeight (1 :: Size Abs))
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
              lineHeight (1 :: Size Abs))
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
              lineHeight (1 :: Size Abs))
             ["Part of "
             ,with a
                   (do style .=
                         do fontWeight (weight 400)
                            textDecorationLine none
                       attrs .
                         at "href" ?=
                         "http://todomvc.com")
                   ["TodoMVC"]]]

toDoSection :: HTML
toDoSection =
  with section
       (style .=
        do borderTop solid
                     (px 1)
                     (rgb 230 230 230)
           zIndex 2
           position relative)
       []

boxShadows4 :: [(Size a, Size a, Size a, Size a, Color)] -> Css
boxShadows4 =
  prefixed (browsers <> "box-shadow") .
  map (\(a,b,c,d,e) -> a ! b ! c ! d ! e)
