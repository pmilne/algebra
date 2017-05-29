module Powerable where

infixr 8 ^

class Powerable a where
    (^)      :: a -> a -> a
    log      :: a -> a

instance Powerable Int where
    (^)       = (Prelude.^)
    log       = undefined

instance Powerable Double where
    (^)       = (Prelude.**)
    log       = Prelude.log

