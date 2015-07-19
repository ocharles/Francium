{-# LANGUAGE RankNTypes #-}

module Francium.Routing (Route, routeComponent, route) where

import Control.Monad
import Control.FRPNow
import Data.Functor.Identity
import Francium.Component
import VirtualDom

type Route = Now (Behavior (HTML Identity ()))

routeComponent :: (Component component)
               => component
               -> (Output component -> [Event Route])
               -> Route
routeComponent c routing =
  do component <- construct c
     changeOut <-
       foldM (\l r -> sample (first l r))
             never
             (routing (outputs component))
     switches <- planNow changeOut
     pure (switch (observeHTML (render component)) switches)

route :: Route -> Now (HTML Behavior ())
route = fmap embed
