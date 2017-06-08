module Ratio where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd)
import Euclidean
import Field

data Ratio a = Ratio !a !a deriving (Eq, Show, Read)

canonical :: (Ring a, Euclidean a) => (a -> a -> b) -> a -> a -> b
canonical f n d = let g = sign d * gcd n d in f (divideOrFail n g) (divideOrFail d g)

ratio :: (Ring a, Euclidean a) => a -> a -> Ratio a
ratio = canonical Ratio -- Curried constructor

instance (Ring a, Euclidean a) => Additive (Ratio a) where
    (Ratio n1 d1) + (Ratio n2 d2) = ratio (n1 * d2 + n2 * d1) (d1 * d2)
    zero                          = Ratio zero one

instance (Ring a, Euclidean a) => Multiplicative (Ratio a) where
    (Ratio n1 d1) * (Ratio n2 d2) = ratio (n1 * n2) (d1 * d2)
    one                           = Ratio one one

instance (Ring a, Euclidean a) => Negatable (Ratio a) where
    neg (Ratio n d)            = Ratio (neg n) d

instance (Ring a, Euclidean a) => Subtractive (Ratio a) where

instance (Ring a, Euclidean a) => Ring (Ratio a) where

instance (Ring a, Euclidean a) => Field (Ratio a) where
    inv (Ratio n d) = ratio d n -- this could be optimised; there is no need for a gcd in this case -- just signum
