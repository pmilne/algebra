module Ring where

import Additive

infixl 6 -
infixl 7 *

-- https://en.wikipedia.org/wiki/Ring_(mathematics)
class Additive a => Ring a where
    (-)      :: a -> a -> a
    (*)      :: a -> a -> a
    one      :: a
    negate   :: a -> a
--    promote  :: b -> a
    x - y               = x Additive.+ Ring.negate y
    negate x            = Additive.zero Ring.- x

instance Ring Int where
    (*)       = (Prelude.*)
    one       = 1
    negate    = Prelude.negate

instance Ring Integer where
    (*)       = (Prelude.*)
    one       = 1
    negate    = Prelude.negate

instance Ring Double where
    (*)       = (Prelude.*)
    one       = 1
    negate    = Prelude.negate

