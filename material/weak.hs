{-# LANGUAGE MagicHash, UnboxedTuples #-}
import Control.Monad
import Control.Concurrent
import Data.IORef
import System.Mem.Weak
import qualified GHC.Base as GHC
import qualified GHC.IORef as GHC
import qualified GHC.STRef as GHC
import qualified GHC.Weak as GHC

main :: IO ()
main = do
  ref <- newIORef "Hello"
  weak <- mkWeakIORefValueFinalizer ref True (putStrLn "You shouldn't see me")
  forever (threadDelay maxBound)
  readIORef ref >>= print

mkWeakIORefValueFinalizer :: IORef a -> value -> IO () -> IO (Weak value)
mkWeakIORefValueFinalizer r@(GHC.IORef (GHC.STRef r#)) v f = GHC.IO $ \s ->
  case GHC.mkWeak# r# v f s of (# s1, w #) -> (# s1, GHC.Weak w #)
