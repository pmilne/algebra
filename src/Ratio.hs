module Ratio where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd)
import Ring
import Euclidean
import Field

data Ratio a = Ratio !a !a deriving (Eq, Show, Read)

instance (Ring a, Euclidean a) => Additive (Ratio a) where
    (Ratio n1 d1) + (Ratio n2 d2) = canonical (n1 * d2 + n2 * d1) (d1 * d2) Ratio
    zero                          = Ratio zero one
--    promote a = a :/ 0

instance (Multiplicative a) => Multiplicative (Ratio a) where
    one                           = Ratio one one
--    promote a = a :/ 0

instance (Ring a, Euclidean a) => Ring (Ratio a) where
    (Ratio n1 d1) * (Ratio n2 d2) = canonical (n1 * n2) (d1 * d2) Ratio
    negate (Ratio n d)            = Ratio (negate n) d
--    promote a = a :/ 0

instance (Ring a, Euclidean a) => Field (Ratio a) where
    inv (Ratio n d) = canonical d n Ratio -- this could be optimised; there is no need for a gcd in this case
