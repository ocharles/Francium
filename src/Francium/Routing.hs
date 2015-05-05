{-# LANGUAGE RankNTypes #-}

module Francium.Routing (Route, routeComponent, route) where

import Reactive.Banana hiding (Identity)
import Reactive.Banana.Frameworks
import VirtualDom
import Francium.Component
import Data.Functor.Identity

type Route = FrameworksMoment (AnyMoment Behavior (HTML Identity))

routeComponent :: (Component component,TrimOutput component)
               => (forall t. component t)
               -> (forall t. Output (Behavior t) (Event t) component -> [Event t Route])
               -> Route
routeComponent c routing =
  FrameworksMoment
    (do component <- construct c
        switches <-
          execute (unions (routing (outputs component)))
        trim (switchB (observeHTML (render component)) switches))

route :: Frameworks t => Route -> Moment t (HTML (Behavior t))
route (FrameworksMoment m) = fmap embed (m >>= now)
