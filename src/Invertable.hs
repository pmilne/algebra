module Invertable where

class Invertable a where
    reciprocal   :: a -> a

instance Invertable Double where
    reciprocal x = 1 / x
