{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}

import ClearCompleted
import Control.Lens ((?=), (.=))
import Control.Monad
import Data.Monoid
import Francium
import Francium.CSS
import Francium.HTML
import IdiomExp
import NewItemAdder
import OpenItemCount
import Prelude hiding (div, span)
import StateFilter
import ToDoList
import ToggleAll
import Control.FRPNow
import Francium.Component

-- | 'app' defines our TodoMVC clone's top-level definition. To Francium, web
-- applications are simply time-varying HTML documents, and we can see this from
-- the type of 'app' - after building an event network in the 'Moment' monad, it
-- returns a single 'Behavior' containing 'HTML' values.
--
-- Inside our 'app' function, we simply build all the necessary components for
-- the TodoMVC application, and wire them together appropriately.
app :: Now (HTML Behavior)
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
        (embed $(i [|appView $(i [|AppView {avNewItemAdder =
                                              observeHTML (render newItemAdder)
                                           ,avToDoList =
                                              observeHTML (render toDoList)
                                           ,avHasItems =
                                              -- $(i [|(not . null) (allItems (outputs toDoList))|])
                                              fmap (not . null)
                                                   (allItems (outputs toDoList))
                                           ,avToDoSummary =
                                              $(i [|ToDoSummary {tdsOpenItemCounter =
                                                                   observeHTML (render openItemCount)
                                                                ,tdsStateFilter =
                                                                   observeHTML (render stateFilter)
                                                                ,tdsClearCompleted =
                                                                   observeHTML (render clearCompleted)}|])
                                           ,avToggleAll =
                                              observeHTML (render toggleAll)}|])|]))

-- | Now that we have declared our application's event network, the only
-- remaining step is to execute it. To do that, we simply apply 'react' to
-- 'app'. 'react' will watch 'app's HTML 'Behavior', and - whenever it changes -
-- render this into the browsers DOM.
main :: IO ()
main = react app

data AppView m =
  AppView {avNewItemAdder :: HTML m
          ,avToDoList :: HTML m
          ,avHasItems :: Bool
          ,avToDoSummary :: ToDoSummary m
          ,avToggleAll :: HTML m}

-- | The 'appView' function simply stiches together all renderings of child
-- components into the main HTML of the document. Francium encourages the use
-- of inline styles, but that requires discipline in making small functions.
-- Here we see that most of the elements that we're adding content to are
-- abstract HTML values.
appView :: Applicative m => AppView m -> HTML m
appView AppView{..} =
  into mainContainer
       (mconcat [into rootSection
                      (header_ (mconcat [pageTitle,avNewItemAdder]) <>
                       if avHasItems
                          then mconcat [toDoSection
                                          (mconcat [avToggleAll,avToDoList])
                                       ,toDoSummary avToDoSummary]
                          else mempty)
                ,pageFooter])

-- | The application itself is rendered into the <body> of the document. Here
-- we style the body accordingly.
mainContainer :: Applicative m => HTML m
mainContainer =
  body_
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
       mempty

-- | The root section contains the application, and gives the distinctive
-- todo-mvc container look.
rootSection :: Applicative m => HTML m
rootSection =
  section_
       (style .=
        do boxShadows
             [(px 0,px 2,px 4,rgba 0 0 0 51),(px 0,px 25,px 50,rgba 0 0 0 25)]
           position relative
           margin (px 130)
                  (px 0)
                  (px 40)
                  (px 0)
           backgroundColor (rgb 255 255 255))
       mempty

-- | The to-do section contains the to-do list, and also overlaps the
-- "complete all" checkbox.
toDoSection :: Applicative m => HTML m -> HTML m
toDoSection =
  section_
       (style .=
        do borderTop solid
                     (px 1)
                     (rgb 230 230 230)
           zIndex 2
           position relative)

-- | The page title is a header that shows the user they are viewing the todo-mvc
-- application.
pageTitle :: Applicative m => HTML m
pageTitle =
  h1_
       (style .=
        do color (rgba 175 47 47 39)
           textAlign (alignSide sideCenter)
           fontWeight (weight 100)
           fontSize (px 100)
           width (pct 100)
           top (px (-155))
           position absolute)
       "todos"

-- | The ToDoSummary provides a little information under the to-do list,
-- allowing the user to either filter the to-do list, see how many incomplete
-- items they have, and to clear all completed items.
data ToDoSummary m =
  ToDoSummary {tdsOpenItemCounter :: HTML m
              ,tdsStateFilter :: HTML m
              ,tdsClearCompleted :: HTML m}

toDoSummary :: Applicative m => ToDoSummary m -> HTML m
toDoSummary ToDoSummary{..} =
  footer_ (style .=
           do borderTopColor (rgb 230 230 230)
              borderTopStyle solid
              borderTopWidth (px 1)
              textAlign (alignSide sideCenter)
              height (px 20)
              sym2 padding
                   (px 10)
                   (px 15)
              color (rgb 119 119 119))
          (mconcat [div_ (style .=
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
                         mempty
                   ,tdsOpenItemCounter
                   ,tdsStateFilter
                   ,tdsClearCompleted
                   ,button_ (style .=
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
                            mempty])

pageFooter :: Applicative m => HTML m
pageFooter =
  footer_ (style .=
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
          (mconcat [p_ (style .= lineHeight (1 :: Size Abs)) "Double-click to edit a todo"
                   ,p_ (style .= lineHeight (1 :: Size Abs))
                       ("Template by " <>
                        a_ (do style .=
                                 do fontWeight (weight 400)
                                    textDecorationLine none
                               href_ ?= "http://sindresorhus.com")
                           "Sindre Sorhus")
                   ,p_ (style .= lineHeight (1 :: Size Abs))
                       ("Created by " <>
                        a_ (do style .=
                                 do fontWeight (weight 400)
                                    textDecorationLine none
                               href_ ?= "http://todomvc.com")
                           "you")
                   ,p_ (style .= lineHeight (1 :: Size Abs))
                       ("Part of " <>
                        a_ (do style .=
                                 do fontWeight (weight 400)
                                    textDecorationLine none
                               href_ ?= "http://todomvc.com")
                           "TodoMVC")])

boxShadows4 :: [(Size a, Size a, Size a, Size a, Color)] -> Css
boxShadows4 =
  prefixed (browsers <> "box-shadow") .
  map (\(a,b,c,d,e) -> a ! b ! c ! d ! e)
