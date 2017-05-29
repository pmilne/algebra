module Exponentiable where

infixr 8 ^

class Exponentiable a where
    (^)      :: a -> a -> a
    ln       :: a -> a

instance Exponentiable Int where
    (^)       = (Prelude.^)
    ln        = undefined

instance Exponentiable Double where
    (^)       = (Prelude.**)
    ln        = Prelude.log

