{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module ToDoList where

import Control.Arrow ((&&&))
import Control.Lens ((?=), at)
import Data.Maybe
import Data.Traversable (for)
import Francium
import Francium.Component
import Francium.HTML hiding (i, map)
import GHCJS.Foreign
import GHCJS.Types
import Reactive.Banana
import ToDoItem
import qualified Storage

data ToDoList =
  ToDoList {addItem :: AnyMoment Event JSString
           ,setStatuses :: AnyMoment Event Status
           ,statusFilter :: AnyMoment Behavior (Status -> Bool)
           ,clearCompleted :: AnyMoment Event ()}

instance Component ToDoList where
  data Output behavior event ToDoList = ToDoListOutput{allItems ::
                                                     behavior [Status]}
  construct tdi =
    mdo addItemNow <-
          fmap (filterE (not . isEmptyString . fromJSString))
               (now (addItem tdi))
        clearCompletedNow <-
          now (clearCompleted tdi)
        statusFilterNow <- now (statusFilter tdi)
        eAddItem <-
          execute (fmap (\x ->
                           FrameworksMoment
                             (trimComponent =<<
                              construct (ToDoItem x (setStatuses tdi))))
                        addItemNow)
        openingStorage <-
          liftIO (fmap (fromMaybe []) Storage.retrieve)
        initialItems <-
          for openingStorage
              (\item ->
                 do component <-
                      trimComponent =<<
                      construct (ToDoItem (toJSString (Storage.title item))
                                          (setStatuses tdi))
                    return (component
                           ,(if Storage.complete item
                                then Complete
                                else Incomplete)))
        startingView <-
          fmap sequenceA
               (for initialItems
                    (\(item,_) ->
                       do render_ <-
                            now (render item)
                          status_ <-
                            now (status (outputs item))
                          return (liftA2 (,) render_ status_)))
        let eItemsChanged =
              accumE (map fst initialItems)
                     (unions [fmap append eAddItem
                             ,destroyItem
                             ,fmap const (incompleteItems <@ clearCompletedNow)])
            incompleteItems =
              switchB (pure (map fst (filter (((== Incomplete) . snd)) initialItems)))
                      (fmap (fmap (map snd .
                                   filter ((== Incomplete) . fst)))
                            (fmap (sequenceA .
                                   map (\item ->
                                          fmap (id &&& const item)
                                               (status (outputs item))))
                                  eItemsChanged))
            items =
              switchB startingView
                      (fmap (sequenceA .
                             fmap (\item ->
                                     liftA2 (,)
                                            (render item)
                                            (status (outputs item))))
                            eItemsChanged)
            destroyItem =
              switchE (fmap (\events ->
                               anyMoment (fmap (unions .
                                                (zipWith (\i -> (deleteElem i <$))
                                                         [0 ..]))
                                               (mapM now events)))
                            (fmap (map (ToDoItem.destroy . outputs)) eItemsChanged))
            visibleItems =
              liftA2 (\f ->
                        map fst .
                        (filter (f . snd)))
                     statusFilterNow
                     items
            stableData =
              switchB (pure openingStorage)
                      (fmap (traverse (\item ->
                                         liftA2 Storage.ToDoItem
                                                (fmap fromJSString (steppedContent (outputs item)))
                                                (fmap (== Complete) (status (outputs item)))))
                            eItemsChanged)
        stableDataChanged <- changes stableData
        reactimate' (fmap (fmap Storage.store) stableDataChanged)
        return Instantiation {render =
                                fmap (into toDoContainer .
                                      map (into itemContainer . pure))
                                     visibleItems
                             ,outputs =
                                ToDoListOutput {allItems =
                                                  fmap (fmap snd) items}}

itemContainer :: HTML
itemContainer =
  with li
       (attrs .
        at "style" ?=
        "border-bottom-color: rgb(237, 237, 237); border-bottom-style: none; border-bottom-width: 1px; font-size: 24px; position: relative;")
       []

toDoContainer :: HTML
toDoContainer =
  with ul
       (do attrs .
             at "style" ?=
             "list-style-type: none; padding: 0px; margin: 0px;")
       []


isEmptyString :: JSString -> Bool
isEmptyString x = null (fromJSString x :: String)


append :: a -> [a] -> [a]
append x xs = xs ++ [x]

deleteElem :: Int -> [a] -> [a]
deleteElem _ [] = []
deleteElem i (x:xs)
  | i < 0 = xs
  | i > length xs = xs
  | i == 0 = xs
  | otherwise = x : deleteElem (i - 1) xs
