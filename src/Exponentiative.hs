module Exponentiative where

infixr 8 ^

class Exponentiative a where
    (^)      :: a -> a -> a
    log      :: a -> a

instance Exponentiative Int where
    (^)       = (Prelude.^)
    log       = undefined

instance Exponentiative Double where
    (^)       = (Prelude.**)
    log       = Prelude.log

