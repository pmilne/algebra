module Ring(module Additive, module Multiplicative, module Ring) where

import Additive
import Multiplicative

infixl 6 -

-- https://en.wikipedia.org/wiki/Ring_(mathematics)
class (Additive a, Multiplicative a) => Ring a where
    (-)      :: a -> a -> a
    negate   :: a -> a
--    promote  :: b -> a
    x - y               = x Additive.+ Ring.negate y
    negate x            = Additive.zero Ring.- x

instance Ring Int where
    negate    = Prelude.negate

instance Ring Integer where
    negate    = Prelude.negate

instance Ring Double where
    negate    = Prelude.negate

