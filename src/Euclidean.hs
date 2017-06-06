module Euclidean where

class Euclidean a where
    quo          :: a -> a -> a
    rem          :: a -> a -> a
    gcd          :: a -> a -> a
    divideOrFail :: a -> a -> a
    sign         :: a -> a
    canonical    :: (a -> a -> b) -> a -> a -> b

instance Euclidean Int where
    quo               = Prelude.quot
    rem               = Prelude.rem
    gcd               = Prelude.gcd
    divideOrFail n d  = if Prelude.rem n d /= 0 then error ("Fail: " ++ show n ++ " // by " ++ show d) else quot n d
    sign              = Prelude.signum
    canonical f n d   = let g = sign d * Prelude.gcd n d in f (Prelude.quot n g) (Prelude.quot d g)

instance Euclidean Integer where
    quo               = Prelude.quot
    rem               = Prelude.rem
    gcd               = Prelude.gcd
    divideOrFail n d  = if Prelude.rem n d /= 0 then error ("Fail: " ++ show n ++ " // by " ++ show d) else quot n d
    sign              = Prelude.signum
    canonical f n d   = let g = sign d * Euclidean.gcd n d in f (Prelude.quot n g) (Prelude.quot d g)

