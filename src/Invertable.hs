module Invertable where

class Invertable a where
    inv   :: a -> a

instance Invertable Double where
    inv x = 1 / x
