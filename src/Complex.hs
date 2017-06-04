{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Complex where

import Prelude hiding ((+), (-), negate, (*), (^), (/))
import Composite
import Field

infix  6  :+

data Complex a = !a :+ !a deriving (Eq, Show, Read)

conjugate :: Ring a => Complex a -> Complex a
conjugate (x :+ y) =  x :+ neg y

instance (Additive a) => Composite (Complex a) a where
    promote a = a :+ zero

instance (Additive a) => Additive (Complex a) where
    (r1 :+ i1) + (r2 :+ i2) = (r1 + r2) :+ (i1 + i2)
    zero                    = zero :+ zero

instance (Subtractive a) => Subtractive (Complex a) where
    neg (r :+ i)         = neg r :+ neg i
--    promote a = a :+ 0

instance (Ring a) => Multiplicative (Complex a) where
    (r1 :+ i1) * (r2 :+ i2) = (r1 * r2 - i1 * i2) :+ (r1 * i2 + i1 * r2)
    one                     = one  :+ zero

instance (Ring a) => Ring (Complex a) where

instance (Field a) => Field (Complex a) where
    inv (a :+ b)            =  let d = a * a + b * b in a / d :+ neg b / d

