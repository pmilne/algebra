{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Ratio where

import Ring

infix  6  :/

data Ratio a = !a :/ !a deriving (Eq, Show, Read)

instance  (Ring a) => Ring (Ratio a) where
    (r1 :/ i1) + (r2 :/ i2)   =  (r1 Ring.+ r2) :/ (i1 Ring.+ i2)
    (r1 :/ i1) * (r2 :/ i2)   =  (r1 Ring.* r2 Ring.- i1 Ring.* i2) :/ (r1 Ring.* i2 Ring.+ i1 Ring.* r2)
    (r1 :/ i1) ^ (r2 :/ i2)   =  undefined
    negate (r:/i)             =  Ring.negate r :/ Ring.negate i
    zero     = Ring.zero :/ Ring.one
    one     = Ring.one :/ Ring.one
--    promote a = a :/ 0
