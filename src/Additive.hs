module Additive where

infixl 6 +

-- https://en.wikipedia.org/wiki/Ring_(mathematics)
class Additive a where
    (+)      :: a -> a -> a
    zero     :: a

instance Additive Int where
    (+)       = (Prelude.+)
    zero      = 0

instance Additive Integer where
    (+)       = (Prelude.+)
    zero      = 0

instance Additive Double where
    (+)       = (Prelude.+)
    zero      = 0

