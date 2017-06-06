module Euclidean where

import Multiplicative

class Multiplicative a => Euclidean a where
    quo          :: a -> a -> a
    rem          :: a -> a -> a
    gcd          :: a -> a -> a
    divideOrFail :: a -> a -> a
    sign         :: a -> a
    canonical    :: (a -> a -> b) -> a -> a -> b
    canonical f n d = let g = sign d Multiplicative.* Euclidean.gcd n d in f (divideOrFail n g) (divideOrFail d g)

instance Euclidean Int where
    quo               = Prelude.quot
    rem               = Prelude.rem
    gcd               = Prelude.gcd
    divideOrFail n d  = if Prelude.rem n d /= 0 then error ("Fail: " ++ show n ++ " // by " ++ show d) else quot n d
    sign              = Prelude.signum

instance Euclidean Integer where
    quo               = Prelude.quot
    rem               = Prelude.rem
    gcd               = Prelude.gcd
    divideOrFail n d  = if Prelude.rem n d /= 0 then error ("Fail: " ++ show n ++ " // by " ++ show d) else quot n d
    sign              = Prelude.signum

