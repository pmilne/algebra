module Subtractive where

import Additive

infixl 6 -

-- https://en.wikipedia.org/wiki/Ring_(mathematics)
class (Additive a) => Subtractive a where
    (-)      :: a -> a -> a
    neg      :: a -> a
--    promote  :: b -> a
    x - y               = x Additive.+ Subtractive.neg y
    neg x               = Additive.zero Subtractive.- x

instance Subtractive Int where
    neg    = Prelude.negate

instance Subtractive Integer where
    neg    = Prelude.negate

instance Subtractive Double where
    neg    = Prelude.negate

