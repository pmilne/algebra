module Exponentiable where

import Ring

infixr 8 ^

class Ring a => Exponentiable a where
    (^)      :: a -> a -> a

instance Exponentiable Int where
    (^)       = (Prelude.^)

instance Exponentiable Double where
    (^)       = (Prelude.**)

