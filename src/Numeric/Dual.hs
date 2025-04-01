{-# LANGUAGE RankNTypes #-}

module Numeric.Dual
    ( Dual(..)
    , derivative
    , realPart
    , dualPart
    ) where

import Numeric

data Dual a = Dual !a !a

instance (Num a) => Num (Dual a) where
    Dual u u' + Dual v v' = Dual (u + v) (u' + v')
    Dual u u' - Dual v v' = Dual (u - v) (u' - v')
    Dual u u' * Dual v v' = Dual (u * v) (u' * v + u * v')
    negate (Dual u u') = Dual (negate u) (negate u')
    abs (Dual u u') = Dual (abs u) (u' * signum u)
    signum (Dual u _) = Dual (signum u) 0
    fromInteger i = Dual (fromInteger i) 0

instance (Fractional a) => Fractional (Dual a) where
    recip (Dual u u') = Dual recip_u (-u' * recip_u * recip_u) where recip_u = recip u
    Dual u u' / Dual v v' = Dual (u / v) ((u' * v - u * v') / (v * v))
    fromRational r = Dual (fromRational r) 0

instance (Floating a) => Floating (Dual a) where
    -- TODO: implement more methods?
    pi = Dual pi 0
    exp (Dual u u') = Dual exp_u (u' * exp_u) where exp_u = exp u
    log (Dual u u') = Dual (log u) (u' / u)
    sqrt (Dual u u') = Dual (sqrt_u) (u' / (2 * sqrt_u)) where sqrt_u = sqrt u
    sin (Dual u u') = Dual (sin u) (u' * cos u)
    cos (Dual u u') = Dual (cos u) (-u' * sin u)
    tan (Dual u u') = Dual tan_u (u' * (1 + tan_u * tan_u)) where tan_u = tan u
    asin (Dual u u') = Dual (asin u) (u' / sqrt (1 - u * u))
    acos (Dual u u') = Dual (acos u) (-u' / sqrt (1 - u * u))
    atan (Dual u u') = Dual (atan u) (u' / (1 + u * u))
    sinh (Dual u u') = Dual (sinh u) (u' * cosh u)
    cosh (Dual u u') = Dual (cosh u) (u' * sinh u)
    tanh (Dual u u') = Dual tanh_u (u' * (1 - tanh_u * tanh_u)) where tanh_u = tanh u
    asinh (Dual u u') = Dual (asinh u) (u' / sqrt (u * u + 1))
    acosh (Dual u u') = Dual (acosh u) (u' / sqrt (u * u - 1))
    atanh (Dual u u') = Dual (atanh u) (u' / (1 - u * u))
    log1p (Dual u u') = Dual (log1p u) (u' / (1 + u))
    expm1 (Dual u u') = Dual (expm1 u) (u' * exp u)

realPart :: Dual a -> a
realPart (Dual u _) = u

dualPart :: Dual a -> a
dualPart (Dual _ u') = u'

derivative :: (Floating a) => (forall b. Floating b => b -> b) -> a -> a
derivative f x = dualPart (f (Dual x 1))
