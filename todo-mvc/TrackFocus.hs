{-# LANGUAGE TypeFamilies #-}

module TrackFocus where

import Francium
import Francium.Hooks

newFocusTracker :: Frameworks t => Moment t (Hook, Event t ())
newFocusTracker = newBlurHook
