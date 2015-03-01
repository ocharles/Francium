{-# LANGUAGE TypeFamilies #-}

module KeyPressObserver where

import Francium
import Francium.Hooks

newKeyPressObserver :: Frameworks t => Moment t (Hook, Event t Int)
newKeyPressObserver = newKeyPressHook
