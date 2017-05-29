module Exponentiable where

infixr 8 ^

class Exponentiable a where
    (^)      :: a -> a -> a
    log      :: a -> a

instance Exponentiable Int where
    (^)       = (Prelude.^)
    log       = undefined

instance Exponentiable Double where
    (^)       = (Prelude.**)
    log       = Prelude.log

