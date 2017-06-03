module Modular where

import Prelude hiding ((+), (-), negate, rem, (*), (^), (/))
import Euclidean
import Ring
import Field

m :: Modular Int
m = Modular 3

instance ModularN Int where
  modulus = 4

data Modular a = Modular !a deriving (Eq, Show, Read)

class ModularN a where
    modulus      :: a

reduce :: Euclidean a => a -> a -> Modular a
reduce a m = Modular (rem a m)

instance (ModularN a, Euclidean a, Ring a) => Ring (Modular a) where
    Modular m1 + Modular m2 = reduce (m1 + m2) modulus
    Modular m1 * Modular m2 = reduce (m1 * m2) modulus
    negate (Modular m)      = Modular (negate m)
    zero                    = Modular zero
    one                     = Modular one

instance (ModularN a, Euclidean a, Ring a) => Field (Modular a) where
    inv (Modular m1)        = undefined -- Modular (inverse11 m1)
