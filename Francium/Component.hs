{-# LANGUAGE TypeFamilies #-}
module Francium.Component
       (Component(..), Instantiated, trimComponent, FutureInstantiation,
        TrimOutput(..), Instantiation(..))
       where

import Reactive.Banana
import Reactive.Banana.Frameworks
import Francium.HTML

data Instantiation behavior event a =
  Instantiation {render :: behavior HTML
                ,outputs :: Output behavior event a}

class Component a where
  data Output (behavior :: * -> *) (event :: * -> *) a :: *
  construct :: Frameworks t => a -> Moment t (Instantiated t a)

type Instantiated t a = Instantiation (Behavior t) (Event t) a
type FutureInstantiation a = Instantiation (AnyMoment Behavior) (AnyMoment Event) a

class TrimOutput a  where
  trimOutput :: Output (Behavior t) (Event t) a
             -> Moment t (Output (AnyMoment Behavior) (AnyMoment Event) a)

trimComponent :: TrimOutput a => Instantiated t a -> Moment t (FutureInstantiation a)
trimComponent (Instantiation r o) = Instantiation <$> trimB r <*> trimOutput o
