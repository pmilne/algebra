module Ratio where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd)
import Ring

infix  6  :/

data Ratio a = !a :/ !a deriving (Eq, Show, Read)

frac :: (Ring a) => a -> a -> Ratio a
frac n d = let g = gcd n d in (n / g) :/ (d / g)

frac1 :: a -> a -> Ratio a
frac1 n d = n :/ d

instance  (Ring a) => Ring (Ratio a) where
    (n1 :/ d1) + (n2 :/ d2)   =  frac1 (n1 * d2 + n2 * d1) (d1 * d2)
    (n1 :/ d1) * (n2 :/ d2)   =  frac1 (n1 * n2 ) (d1 * d2)
    (n1 :/ d1) ^ (n2 :/ d2)   =  undefined
    negate (n :/ d)           =  negate n :/ d
    inv (n :/ d)              =  d :/ n
    zero     = zero :/ one
    one     = one :/ one
--    promote a = a :/ 0
