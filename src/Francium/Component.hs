{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Francium.Component
       (Component(..), Instantiation(..))
       where

import Control.FRPNow
import Francium.HTML

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
  data Output a :: *
  construct :: a -> Now (Instantiation a)

data Instantiation a =
  Instantiation {render :: HTML Behavior
                ,outputs :: Output a}
