{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Francium.Component
       (Component(..), Instantiated, trimComponent, FutureInstantiation,
        TrimOutput(..), Instantiation(..), trim)
       where

import GHC.Generics
import Francium.HTML
import Reactive.Banana
import Reactive.Banana.Frameworks

-- | A 'Component' is a time-varying user interface element. Components expose
-- two elements of public information:
--
--   1. Their 'outputs' - a set of time-varying values (behaviors) or discrete
--      events.
--   2. Their 'render'ing to a 'HTML' tree.
--
-- As each component may expose a different set of outputs (or none), the outpus
-- associated with the component are recorded in an associated data type.
class Component a where
  data Output (behavior :: * -> *) (event :: * -> *) a :: *
  construct :: Frameworks t => a t -> Moment t (Instantiated t a)


-- | A 'Component' instantiated at a known point in time. Users will most commonly
-- work with 'Instantiated' components, as these are the components we observe
-- right now.
type Instantiated t a = Instantiation (Behavior t) (Event t) a


-- | A 'Component' can be "trimmed" in order to be used a /future/ point in
-- time. This construct allows one to switch in new components dynamically.
--
-- For example, a list of elements may change over time - by using
-- 'trimComponent' we are able to model each element of the list as a
-- 'Component'
trimComponent :: TrimOutput a => Instantiated t a -> Moment t (FutureInstantiation a)
trimComponent (Instantiation r o) =
  Instantiation <$>
  (case r of
     HTML m -> fmap HTML (trimB m)) <*>
  trimOutput o

-- | A future instantiation of a component, to be dynamically switched. You can
-- turn any component into a 'FutureInstantiation' by trimming it with
-- 'trimComponent'.
type FutureInstantiation a = Instantiation (AnyMoment Behavior) (AnyMoment Event) a


class TrimOutput a  where
  -- | Trim the particular outputs of a component. You will need an instance of
  -- this if your component can be trimmed, though the default definition in
  -- terms of GHC Generics will often suffice.
  trimOutput :: Output (Behavior t) (Event t) a
             -> Moment t (Output (AnyMoment Behavior) (AnyMoment Event) a)
  -- Holy contexts, Batman!
  default trimOutput :: (Generic (Output (Behavior t) (Event t) a)
                       ,Generic (Output (AnyMoment Behavior) (AnyMoment Event) a)
                       ,GTrim (Rep (Output (Behavior t) (Event t) a))
                       ,GTrimmed (Rep (Output (Behavior t) (Event t) a)) ~ Rep (Output (AnyMoment Behavior) (AnyMoment Event) a)
                       ,t ~ GTime (Rep (Output (Behavior t) (Event t) a)))
                     => Output (Behavior t) (Event t) a
                     -> Moment t (Output (AnyMoment Behavior) (AnyMoment Event) a)
  trimOutput = fmap to . gtrim . from


data Instantiation behavior event a =
  Instantiation {render :: HTML behavior
                ,outputs :: Output behavior event a}

class GTrim f where
  type GTrimmed f :: * -> *
  type GTime f :: *
  gtrim :: f a -> Moment (GTime f) ((GTrimmed f) a)

instance GTrim f => GTrim (M1 i c f) where
  type GTrimmed (M1 i c f) = M1 i c (GTrimmed f)
  type GTime (M1 i c f) = GTime f
  gtrim = fmap M1 . gtrim . unM1

instance (GTrim f,GTrim g,GTime f ~ GTime g) => GTrim (f :*: g) where
  type GTrimmed (f :*: g) = GTrimmed f :*: GTrimmed g
  type GTime (f :*: g) = GTime f
  gtrim (l :*: r) =
    liftA2 (:*:)
           (gtrim l)
           (gtrim r)

instance Trim c => GTrim (K1 i c) where
  type GTrimmed (K1 i c) = K1 i (Trimmed c)
  type GTime (K1 i c) = Time c
  gtrim = fmap K1 . trim . unK1

class Trim a where
  type Trimmed a :: *
  type Time a :: *
  trim :: a -> Moment (Time a) (Trimmed a)

instance Trim (Behavior t a) where
  type Trimmed (Behavior t a) = AnyMoment Behavior a
  type Time (Behavior t a) = t
  trim = trimB

instance Trim (Event t a) where
  type Trimmed (Event t a) = AnyMoment Event a
  type Time (Event t a) = t
  trim = trimE
