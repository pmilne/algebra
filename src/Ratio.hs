module Ratio where

import Prelude hiding ((+), (-), negate, (*), (^))
import Ring

infix  6  :/

data Ratio a = !a :/ !a deriving (Eq, Show, Read)

instance  (Ring a) => Ring (Ratio a) where
    (r1 :/ i1) + (r2 :/ i2)   =  (r1 + r2) :/ (i1 + i2)
    (r1 :/ i1) * (r2 :/ i2)   =  (r1 * r2 - i1 * i2) :/ (r1 * i2 + i1 * r2)
    (r1 :/ i1) ^ (r2 :/ i2)   =  undefined
    negate (r:/i)             =  negate r :/ i
    zero     = zero :/ one
    one     = one :/ one
--    promote a = a :/ 0
