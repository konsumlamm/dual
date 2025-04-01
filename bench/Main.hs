module Main (main) where

import Test.Tasty.Bench

import Numeric
import Numeric.Dual

main :: IO ()
main = defaultMain
    [ bench "exp" $ whnf exp a
    , bench "log" $ whnf log a
    , bench "sqrt" $ whnf sqrt a
    , bench "sin" $ whnf sin a
    , bench "cos" $ whnf cos a
    , bench "tan" $ whnf tan a
    , bench "asin" $ whnf asin a
    , bench "acos" $ whnf acos a
    , bench "atan" $ whnf atan a
    , bench "sinh" $ whnf sinh a
    , bench "cosh" $ whnf cosh a
    , bench "tanh" $ whnf tanh a
    , bench "asinh" $ whnf asinh a
    , bench "acosh" $ whnf acosh a
    , bench "atanh" $ whnf atanh a
    , bench "log1p" $ whnf log1p a
    , bench "expm1" $ whnf expm1 a
    ]
  where
    a = Dual 1.0 0.5 :: Dual Double
