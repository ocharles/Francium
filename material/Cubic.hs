{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}

import Criterion
import Criterion.Main
import Control.Applicative
import Data.Maybe
import Control.Monad
import Numeric.AD hiding (diff')
import Numeric.AD.Rank1.Forward
import qualified Numeric.AD.Rank1.Forward.Double as FD2
import qualified Numeric.AD.Rank1.Newton as Newton

cubicBezier
  :: (Double,Double) -> (Double,Double) -> Double -> Double -> Double
cubicBezier !(!p1x,!p1y) !(!p2x,!p2y) =
  let cx = 3 * p1x
      bx = 3 * (p2x - p1x) - cx
      ax = 1 - cx - bx
      cy = 3 * p1y
      by = 3 * (p2y - p1y) - cy
      ay = 1 - cy - by
      sampleCurveX !t =
        ((ax * t + bx) * t + cx) * t
      sampleCurveY !t =
        ((ay * t + by) * t + cy) * t
      sampleCurveDerivativeX !t =
        (3 * ax * t + 2 * bx) * t + cx
      solveCurveX !x !epsilon =
        let solveByNewton
              :: Int -> Double -> Maybe Double
            solveByNewton !n !t2 =
              do guard (n < 8)
                 let x2 = sampleCurveX t2 - x
                 if abs x2 < epsilon
                    then return t2
                    else do let d2 = sampleCurveDerivativeX t2
                            guard (abs d2 < 1.0e-6)
                            solveByNewton (n + 1)
                                          (t2 - x2 / d2)
            solveByBisection !t0 !t1 !t2 =
              do guard (t0 < t1)
                 let x2 = sampleCurveX t2
                 if abs (x2 - x) < epsilon
                    then return t2
                    else (let (t0',t1') =
                                if x > x2
                                   then (t2,t1)
                                   else (t0,t2)
                              t2' =
                                (t1' - t0') * 0.5 + t0'
                          in solveByBisection t0' t1' t2' <|> return t2')
        in fromMaybe x (solveByNewton 0 x <|> solveByBisection 0 1 x)
      solveEpsilon !d = 1.0 / (200.0 * d)
  in \duration x ->
       sampleCurveY
         (solveCurveX x
                      (solveEpsilon duration))

solveCubicBezierTForX :: (Double,Double)
                      -> (Double,Double)
                      -> Double
                      -> [Double]
solveCubicBezierTForX p2 p3 x =
  findZero
    (\t -> cubicBezierXAD p2 p3 t - auto x)
    x
  where findZero f = go
          where go x = x : if x == xn then [] else go xn
                  where (y,y') = FD2.diff' f x
                        xn = x - y / y'

cubicBezierX :: (Double,Double) -> (Double,Double) -> Double -> Double
cubicBezierX (p1x,_) (p2x,_) t = ((ax * t + bx) * t + cx) * t
        cx = 3 * p1x
        bx = 3 * (p2x - p1x) - cx
        ax = 1 - cx - bx

cubicBezierXAD
  :: Mode a
  => (Scalar a,Scalar a) -> (Scalar a,Scalar a) -> a -> a
cubicBezierXAD (p2x,_) (p3x,_) t =
  ((ax * t + bx) * t + cx) * t
  where cx = 3 * auto p2x
        bx = 3 * (auto p3x - auto p2x) - cx
        ax = 1 - cx - bx

cubicBezierY
  :: Mode a
  => (Scalar a,Scalar a) -> (Scalar a,Scalar a) -> a -> a
cubicBezierY (_,p2y) (_,p3y) t =
  ((ay * t + by) * t + cy) * t
  where cy = 3 * auto p2y
        by = 3 * (auto p3y - auto p2y) - cy
        ay = 1 - cy - by



main :: IO ()
main =
  defaultMain
    [bgroup "cubicBezierX"
            (map (\t ->
                    bench (show t)
                          (whnf (cubicBezierX (0.23,1)
                                              (0.32,1))
                                t))
                 [0 :: Double])
    ,bgroup "cubicBezierXAD"
            (map (\t ->
                    bench (show t)
                          (whnf (\t ->
                                   fst (diff' (cubicBezierXAD (0.23,1)
                                                              (0.32,1))
                                              t))
                                t))
                 [0 :: Double])
    ,bgroup "cubicBezierXAD2"
            (map (\t ->
                    bench (show t)
                          (whnf (\t ->
                                   fst (FD2.diff' (cubicBezierXAD (0.23,1)
                                                                  (0.32,1))
                                                  t))
                                t))
                 [0 :: Double])
    ,bgroup "cubicBezier"
            (map (\t ->
                    bench (show t)
                          (whnf (cubicBezier (0.23,1)
                                             (0.32,1)
                                             10000)
                                t))
                 [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0])
    ,bgroup "cubicBezierAD Newton"
            (map (\t ->
                    bench (show t)
                          (whnf (cubicBezierAD (0.23,1)
                                               (0.32,1)
                                               10000)
                                t))
                 [0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0])]
