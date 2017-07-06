module Modular where

import Prelude hiding ((+), (-), negate, rem, (*), (^), (/))

import Domains.Euclidean
import Domains.Ring
import Domains.Field

-- Humph, it looks like we need to use explicit reification to associate the modulus with the class.
-- Oh dear. Hardcode the modulus for now...
instance ModularN Int where
  modulus = 4

data Modular a = Modular !a deriving (Eq, Show, Read)

class ModularN a where
    modulus      :: a

reduce :: Euclidean a => a -> a -> Modular a
reduce a m = Modular (rem a m)

instance (ModularN a, Euclidean a, Ring a) => Additive (Modular a) where
    Modular m1 + Modular m2 = reduce (m1 + m2) modulus
    zero                    = Modular zero

instance (ModularN a, Euclidean a, Ring a) => Multiplicative (Modular a) where
    Modular m1 * Modular m2 = reduce (m1 * m2) modulus
    one                     = Modular one

instance (ModularN a, Euclidean a, Ring a) => Negatable (Modular a) where
    neg (Modular m)      = Modular (neg m)

instance (ModularN a, Euclidean a, Ring a) => Subtractive (Modular a) where

instance (ModularN a, Euclidean a, Ring a) => Ring (Modular a) where

instance (ModularN a, Euclidean a, Ring a) => Reciprocative (Modular a) where
    reciprocal (Modular _)        = undefined -- Modular (inverse11 m1)

