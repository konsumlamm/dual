{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Numeric
import Numeric.Dual

default (Int)

infix 4 ~=~
(~=~) :: Double -> Double -> Property
x ~=~ y = counterexample (show x ++ (if res then " == " else " /= ") ++ show y) res
  where
    res = x == y || abs (x - y) <= max (rtol * max (abs x) (abs y)) atol
    rtol = 1e-12
    atol = 1e-12

main :: IO ()
main = defaultMain $ testGroup "Dual"
    [ testProperty "d/dx x ^ n" $ \x0 (Positive n) -> derivative (\x -> x ^ n) x0 ~=~ fromIntegral n * x0 ^ (n - 1)
    , testProperty "d/dx x ^^ n" $ \(NonZero x0) (NonZero n) -> derivative (\x -> x ^^ n) x0 ~=~ fromIntegral n * x0 ^^ (n - 1)
    , testProperty "d/dx sqrt x" $ \(Positive x0) -> derivative sqrt x0 ~=~ derivative (\x -> x ** 0.5) x0
    , testProperty "d/dx log1p x" $ \x0 -> derivative log1p x0 ~=~ derivative (\x -> log (1 + x)) x0
    , testProperty "d/dx expm1 x" $ \x0 -> derivative expm1 x0 ~=~ derivative (\x -> exp x - 1) x0
    ]
