module Euclidean where

import Multiplicative

class Euclidean a where
    quo          :: a -> a -> a
    rem          :: a -> a -> a
    gcd          :: a -> a -> a
    divideOrFail :: a -> a -> a
    sign         :: a -> a

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

