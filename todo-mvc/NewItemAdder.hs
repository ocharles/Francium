{-# LANGUAGE OverloadedStrings #-}

module NewItemAdder (NewItemAdder, niaView, niaComplete, mkNewItemAdder) where

import Francium
import Francium.HTML
import qualified Francium.HTML as HTML
import Control.Lens ((?=), at)
import Prelude hiding (div, span)
import Reactive.Banana
import GHCJS.Foreign
import GHCJS.Types

data NewItemAdder t =
  NewItemAdder {niaView :: Behavior t HTML
               ,niaComplete :: Event t JSString}

mkNewItemAdder :: Frameworks t => Moment t (NewItemAdder t)
mkNewItemAdder =
  do inputEv <- newDOMEvent
     keyPressEv <- newDOMEvent
     let complete =
           itemValue <@
           filterE (== 13) (domEvent keyPressEv)
         itemValue =
           accumB ""
                  ((const <$> domEvent inputEv) `union`
                   (const "" <$
                    complete))
     return NewItemAdder {niaView =
                            fmap (\v ->
                                    with input
                                         (do inputAttributes
                                             onInput inputEv
                                             onKeyPress keyPressEv
                                             HTML.value ?= v)
                                         [])
                                 itemValue
                         ,niaComplete = complete}
  where inputAttributes =
          do attrs .
               at "style" ?=
               "-webkit-font-smoothing: antialiased; box-sizing: border-box; box-shadow: rgba(0, 0, 0, 0.027451) 0px -2px 1px inset; border: 1px none rgb(153, 153, 153); padding: 16px 16px 16px 60px; outline-style: none; line-height: 1.4em; font-size: 24px; width: 100%; margin: 0px; position: relative; background-color: rgba(0, 0, 0, 0);"
             attrs .
               at "placeholder" ?=
               "What needs to be done?"
             attrs .
               at "autofocus" ?=
               ""
