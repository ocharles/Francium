{-# LANGUAGE TypeFamilies #-}

module KeyPressObserver where

import Francium
import Francium.HTML hiding (a, html)
import HoverObserver

newKeyPressObserver :: Frameworks t => Moment t (Hook, Event t Int)
newKeyPressObserver =
  do keyPressEv <- newDOMEvent
     let hook = Hook (onKeyPress keyPressEv)
     return (hook,domEvent keyPressEv)
