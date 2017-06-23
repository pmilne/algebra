module Applicable where

-- Applicative is a central construct in Haskell -- use the name Applicable instead

class Applicable a where
    apply      :: a -> a -> a

instance Applicable Int where
    apply       = undefined

instance Applicable Integer where
    apply       = undefined

instance Applicable Double where
    apply       = undefined

