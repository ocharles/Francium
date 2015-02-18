{-# LANGUAGE TypeFamilies #-}

module TrackFocus where

import Francium
import Francium.HTML hiding (a, html)
import HoverObserver

newFocusTracker :: Frameworks t => Moment t (Hook, Event t ())
newFocusTracker = 
    do blur <- newDOMEvent
       let hook = Hook (onBlur blur)
       return (hook, domEvent blur)
