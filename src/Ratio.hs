module Ratio where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd)
import Additive
import Ring
import Euclidean
import Field

infix  6  :/

data Ratio a = !a :/ !a deriving (Eq, Show, Read)

frac :: (Euclidean a) => a -> a -> Ratio a
frac n d = let g = gcd n d in quo n g :/ quo d  g

instance (Ring a, Euclidean a) => Additive (Ratio a) where
    (n1 :/ d1) + (n2 :/ d2)   = frac (n1 * d2 + n2 * d1) (d1 * d2)
    zero                      = zero :/ one
--    promote a = a :/ 0

instance (Ring a, Euclidean a) => Ring (Ratio a) where
    (n1 :/ d1) * (n2 :/ d2)   = frac (n1 * n2) (d1 * d2)
    negate (n :/ d)           = negate n :/ d
    one                       = one :/ one
--    promote a = a :/ 0

instance (Ring a, Euclidean a) => Field (Ratio a) where
    inv (n :/ d) = d :/ n
