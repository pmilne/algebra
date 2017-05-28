module Euclidean where

class Euclidean a where
    quo      :: a -> a -> a
    rem      :: a -> a -> a
    gcd      :: a -> a -> a
    quo       = undefined
    rem       = undefined
    gcd       = undefined

instance Euclidean Int where
    quo       = Prelude.quot
    rem       = Prelude.rem
    gcd       = Prelude.gcd

