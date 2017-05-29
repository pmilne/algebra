module Complex where

import Prelude hiding ((+), (-), negate, (*), (^), (/))
import Ring
import Field

infix  6  :+

data Complex a = !a :+ !a deriving (Eq, Show, Read)

conjugate        :: Ring a => Complex a -> Complex a
conjugate (x :+ y) =  x :+ negate y

modulus2        :: Ring a => Complex a -> a
modulus2 (x :+ y) =  x * x + y * y

instance (Ring a) => Ring (Complex a) where
    (r1 :+ i1) + (r2 :+ i2)   =  (r1 + r2) :+ (i1 + i2)
    (r1 :+ i1) * (r2 :+ i2)   =  (r1 * r2 - i1 * i2) :+ (r1 * i2 + i1 * r2)
    negate (r:+i)             =  negate r :+ negate i
    zero     = zero :+ zero
    one      = one  :+ zero
--    promote a = a :+ 0

instance (Field a) => Field (Complex a) where
    inv c             =  let m2 = modulus2 c in case c of (a :+ b) -> a / m2 :+ negate b / m2

