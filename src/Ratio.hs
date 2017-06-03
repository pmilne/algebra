module Ratio where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd)
import Additive
import Ring
import Euclidean
import Field

data Ratio a = Ratio !a !a deriving (Eq, Show, Read)

frac :: (Euclidean a) => a -> a -> Ratio a
frac n d = let g = gcd n d in Ratio (quo n g) (quo d  g)

instance (Ring a, Euclidean a) => Additive (Ratio a) where
    (Ratio n1 d1) + (Ratio n2 d2)   = frac (n1 * d2 + n2 * d1) (d1 * d2)
    zero                      = Ratio zero one
--    promote a = a :/ 0

instance (Ring a, Euclidean a) => Ring (Ratio a) where
    (Ratio n1 d1) * (Ratio n2 d2) = frac (n1 * n2) (d1 * d2)
    negate (Ratio n d)            = Ratio (negate n) d
    one                           = Ratio one one
--    promote a = a :/ 0

instance (Ring a, Euclidean a) => Field (Ratio a) where
    inv (Ratio n d) = Ratio d n -- fixme sign!!!!
