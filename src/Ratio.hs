module Ratio where

import Prelude hiding ((+), (-), negate, (*), (^))
import Ring

infix  6  :/

data Ratio a = !a :/ !a deriving (Eq, Show, Read)

instance  (Ring a) => Ring (Ratio a) where
    (n1 :/ d1) + (n2 :/ d2)   =  (n1 + n2) :/ (d1 + d2)
    (n1 :/ d1) * (n2 :/ d2)   =  (n1 * n2 - d1 * d2) :/ (n1 * d2 + d1 * n2)
    (n1 :/ d1) ^ (n2 :/ d2)   =  undefined
    negate (n :/ d)             =  negate n :/ d
    zero     = zero :/ one
    one     = one :/ one
--    promote a = a :/ 0
