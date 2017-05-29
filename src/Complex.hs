module Complex where

import Prelude hiding ((+), (-), negate, (*), (^), (/))
import Ring
import Field

infix  6  :+

data Complex a = !a :+ !a deriving (Eq, Show, Read)

conjugate        :: Ring a => Complex a -> Complex a
conjugate (x :+ y) =  x :+ negate y

instance (Ring a) => Ring (Complex a) where
    (r1 :+ i1) + (r2 :+ i2) = (r1 + r2) :+ (i1 + i2)
    (r1 :+ i1) * (r2 :+ i2) = (r1 * r2 - i1 * i2) :+ (r1 * i2 + i1 * r2)
    negate (r :+ i)         = negate r :+ negate i
    zero                    = zero :+ zero
    one                     = one  :+ zero
--    promote a = a :+ 0

instance (Field a) => Field (Complex a) where
    inv (a :+ b)            =  let d = a * a + b * b in a / d :+ negate b / d

