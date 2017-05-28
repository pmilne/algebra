module Euclidean where

class Euclidean a where
    quo      :: a -> a -> a
    rem      :: a -> a -> a
    gcd      :: a -> a -> a

instance Euclidean Int where
    quo       = Prelude.quot
    rem       = Prelude.rem
    gcd       = Prelude.gcd

