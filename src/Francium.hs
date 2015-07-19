module Francium where

import Control.FRPNow
import Control.Monad ((<=<))
import Control.Monad.Trans.Writer.Strict
import Data.Foldable
import GHCJS.Foreign
import GHCJS.Types
import VirtualDom

-- import Control.Applicative
-- import Control.Concurrent
-- import Control.Concurrent.STM
-- import Control.Monad.IO.Class
-- import Data.Foldable
-- import Data.IORef
-- import Data.Profunctor
-- import Francium.Component
-- import Francium.HTML
-- import Francium.Hooks
-- import Francium.Routing
-- import GHCJS.Foreign
-- import GHCJS.Types
-- import Prelude hiding (div, mapM, sequence)
-- import Reactive.Banana
-- import Reactive.Banana.Frameworks
-- import VirtualDom
-- import qualified VirtualDom.Prim as VDom

--------------------------------------------------------------------------------
react :: Now (HTML Behavior ()) -> IO ()
react app =
  do container <- newTopLevelContainer
     _ <- initDomDelegator
     runNowMaster
       (do document <-
             fmap (\x ->
                     case div_ x of
                       HTML (WriterT beh) ->
                         fmap (head . toList . snd) beh)
                  app
           initialDocument <- sample document
           sync (nextTick (renderTo container initialDocument))
           callIOStream (nextTick . renderTo container)
                        (toChanges document)
           pure never)

--------------------------------------------------------------------------------
foreign import javascript unsafe
  "window.nextTick($1)"
  ffiNextTick :: JSFun (IO ()) -> IO ()

nextTick :: IO () -> IO ()
nextTick = ffiNextTick <=< syncCallback AlwaysRetain True
