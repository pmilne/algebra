{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Complex where

import Prelude hiding ((+), (-), negate, (*), (^))
import Ring

infix  6  :+

data Complex a = !a :+ !a deriving (Eq, Show, Read)

conjugate        :: Num a => Complex a -> Complex a
conjugate (x:+y) =  x :+ (-y)

instance  (Ring a) => Ring (Complex a) where
    (r1 :+ i1) + (r2 :+ i2)   =  (r1 + r2) :+ (i1 + i2)
    (r1 :+ i1) * (r2 :+ i2)   =  (r1 * r2 - i1 * i2) :+ (r1 * i2 + i1 * r2)
    (r1 :+ i1) ^ (r2 :+ i2)   =  undefined
    negate (r:+i)             =  negate r :+ negate i
    zero     = zero :+ zero
    one      = one  :+ zero
--    promote a = a :+ 0
