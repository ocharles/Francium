{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}

module ToDoList where

import Clay.Border
import Clay.Color
import Clay.Common as Css
import Clay.Display
import Clay.Font
import Clay.Geometry
import Clay.List
import Clay.Size
import Control.Arrow ((&&&))
import Control.Lens ((.=))
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

data ToDoList t =
  ToDoList {addItem :: Event t JSString
           ,setStatuses :: Event t Status
           ,statusFilter :: Behavior t (Status -> Bool)
           ,clearCompleted :: Event t ()}

instance Component ToDoList where
  data Output behavior event ToDoList = ToDoListOutput{allItems ::
                                                     behavior [Status]}
  construct tdi =
    mdo let addNonEmptyItem =
              filterE (not . isEmptyString . fromJSString)
                      (addItem tdi)
        eAddItem <-
          do setStatusesLater <-
               trimE (setStatuses tdi)
             execute (fmap (\x ->
                              FrameworksMoment
                                (do setStatuses' <- now setStatusesLater
                                    trimComponent =<<
                                      construct (ToDoItem x setStatuses')))
                           addNonEmptyItem)
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
                             ,fmap const (incompleteItems <@ clearCompleted tdi)])
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
                     (statusFilter tdi)
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
       (style .=
        do borderBottomColor (rgb 237 237 237)
           borderBottomStyle none
           borderBottomWidth (px 1)
           fontSize (px 24)
           position relative)
       []

toDoContainer :: HTML
toDoContainer =
  with ul
       (style .=
        do listStyleType none
           sym padding (px 0)
           sym margin (px 0))
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
