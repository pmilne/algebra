module Exponentiative where

infixr 8 ^

class Exponentiative a where
    (^)      :: a -> a -> a
    log      :: a -> a -> a
    exp      :: a -> a
    ln       :: a -> a
    sqrt     :: a -> a
    two      :: a -- oh dear, this was all going so well

instance Exponentiative Int where
    (^)       = (Prelude.^)
    log       = undefined
    exp       = undefined
    ln        = undefined
    sqrt      = undefined
    two       = 2

instance Exponentiative Double where
    (^)       = (Prelude.**)
    log       = Prelude.logBase
    exp       = Prelude.exp
    ln        = Prelude.log
    sqrt      = Prelude.sqrt
    two       = 2

