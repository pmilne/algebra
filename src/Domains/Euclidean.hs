module Domains.Euclidean where

--import Prelude hiding (return)
import Domains.Additive

-- Accessor-like functions for the rtn argument of divide

_quo :: t -> t -> t
_quo q r = q

_rem :: t -> t -> t
_rem q r = r

infixl 7 /! -- divide or fail

(/!) :: Euclidean a => a -> a -> a
(/!) = divideOrFail

class (Show a, Eq a, Additive a) => Euclidean a where
    quo               :: a -> a -> a
    rem               :: a -> a -> a
    divide            :: (a -> a -> b) -> a -> a -> b
    gcd               :: a -> a -> a
    divideOrFail      :: a -> a -> a
    sign              :: a -> a

    quo               = divide _quo
    rem               = divide _rem
    divide rtn n d    = rtn (quo n d) (Domains.Euclidean.rem n d)
    divideOrFail n d  = divide (\q r -> if r /= zero then error ("Fail: " ++ show n ++ " // by " ++ show d) else q) n d

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

