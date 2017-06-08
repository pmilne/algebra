module Rational where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd, Rational)
import Euclidean
import Field

data Rational a = Rational !a !a deriving (Eq, Show, Read)

canonical :: (Ring a, Euclidean a) => (a -> a -> b) -> a -> a -> b
canonical f n d = let g = sign d * gcd n d in f (divideOrFail n g) (divideOrFail d g)

ratio :: (Ring a, Euclidean a) => a -> a -> Rational a
ratio = canonical Rational -- Curried constructor

instance (Ring a, Euclidean a) => Additive (Rational a) where
    (Rational n1 d1) + (Rational n2 d2) = ratio (n1 * d2 + n2 * d1) (d1 * d2)
    zero                          = Rational zero one

instance (Ring a, Euclidean a) => Multiplicative (Rational a) where
    (Rational n1 d1) * (Rational n2 d2) = ratio (n1 * n2) (d1 * d2)
    one                           = Rational one one

instance (Ring a, Euclidean a) => Negatable (Rational a) where
    neg (Rational n d)            = Rational (neg n) d

instance (Ring a, Euclidean a) => Subtractive (Rational a) where

instance (Ring a, Euclidean a) => Ring (Rational a) where

instance (Ring a, Euclidean a) => Invertable (Rational a) where
    inv (Rational n d) = ratio d n -- this could be optimised; there is no need for a gcd in this case -- just signum

instance (Ring a, Euclidean a) => Field (Rational a) where
