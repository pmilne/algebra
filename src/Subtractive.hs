module Subtractive(module Additive, module Negatable, module Subtractive) where

import Prelude hiding ((+), (-), negate, (*), (^), (/))
import Additive
import Negatable

infixl 6 -

class (Additive a, Negatable a) => Subtractive a where
    (-)      :: a -> a -> a
    x - y     = x + neg y

instance Subtractive Int where

instance Subtractive Integer where

instance Subtractive Double where
