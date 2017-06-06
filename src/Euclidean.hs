module Euclidean where

import Prelude hiding (return)
import Additive

quo1 q r = q
rem1 q r = r

class (Show a, Eq a, Additive a) => Euclidean a where
    quo               :: a -> a -> a
    rem               :: a -> a -> a
    divide            :: (a -> a -> b) -> a -> a -> b
    gcd               :: a -> a -> a
    divideOrFail      :: a -> a -> a
    sign              :: a -> a

    quo                = divide quo1
    rem                = divide rem1
    divide return n d  = return (quo n d) (Euclidean.rem n d)
    divideOrFail n d   = divide (\q r -> if r /= zero then error ("Fail: " ++ show n ++ " // by " ++ show d) else q) n d

instance Euclidean Int where
    quo               = Prelude.quot
    rem               = Prelude.rem
    gcd               = Prelude.gcd
    sign              = Prelude.signum

instance Euclidean Integer where
    quo               = Prelude.quot
    rem               = Prelude.rem
    gcd               = Prelude.gcd
    sign              = Prelude.signum

