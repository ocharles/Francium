module Francium.DOMEvent where

import Data.Profunctor
import Reactive.Banana
import Reactive.Banana.Frameworks

data DOMEvent t a b = DOMEvent { domEventHandler :: Handler a, domEvent :: Event t b }

instance Functor (DOMEvent t a) where
  fmap f (DOMEvent input output) = DOMEvent input (f <$> output)

instance Profunctor (DOMEvent t) where
  lmap f (DOMEvent input output) = DOMEvent (input . f) output
  rmap = fmap

newDOMEvent :: Frameworks t => Moment t (DOMEvent t a a)
newDOMEvent = uncurry (flip DOMEvent) <$> newEvent
