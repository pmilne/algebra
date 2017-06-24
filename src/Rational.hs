module Rational(Euclidean, module Rational) where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd, Rational)
import Euclidean
import Field

-- A fraction or ratio between two elements of a Euclidean Ring. E.g. Integer or Polynomial
data Rational a = Rational !a !a deriving (Eq, Read)

canonical :: (Ring a, Euclidean a) => (a -> a -> b) -> a -> a -> b
canonical f n d = let g = sign d * gcd n d in f (divideOrFail n g) (divideOrFail d g)

rational :: (Ring a, Euclidean a) => a -> a -> Rational a
rational = canonical Rational -- Curried constructor

rational1 :: (Ring a, Euclidean a) => a -> Rational a
rational1 n = rational n one

instance (Eq a, Show a, Multiplicative a) => Show (Rational a) where
  show (Rational n d) = if d == one then show n else show n ++ "/" ++ show d

instance (Ring a, Euclidean a) => Additive (Rational a) where
    (Rational n1 d1) + (Rational n2 d2) = rational (n1 * d2 + n2 * d1) (d1 * d2)
    zero                          = Rational zero one

instance (Ring a, Euclidean a) => Multiplicative (Rational a) where
    (Rational n1 d1) * (Rational n2 d2) = rational (n1 * n2) (d1 * d2)
    one                           = Rational one one

instance (Ring a, Euclidean a) => Negatable (Rational a) where
    neg (Rational n d)            = Rational (neg n) d

instance (Ring a, Euclidean a) => Subtractive (Rational a) where

instance (Ring a, Euclidean a) => Ring (Rational a) where

instance (Ring a, Euclidean a) => Invertable (Rational a) where
    reciprocal (Rational n d) = rational d n -- this could be optimised; there is no need for a gcd in this case -- just signum

instance (Ring a, Euclidean a) => Field (Rational a) where
