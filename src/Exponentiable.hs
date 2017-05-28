module Exponentiable where

infixr 8 ^

class Exponentiable a where
    (^)      :: a -> a -> a

instance Exponentiable Int where
    (^)       = (Prelude.^)

instance Exponentiable Double where
    (^)       = (Prelude.**)

