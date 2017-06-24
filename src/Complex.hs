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

instance (Negatable a) => Negatable (Complex a) where
    neg (r :+ i)         = neg r :+ neg i

instance (Subtractive a) => Subtractive (Complex a) where

instance (Ring a) => Multiplicative (Complex a) where
    (r1 :+ i1) * (r2 :+ i2) = (r1 * r2 - i1 * i2) :+ (r1 * i2 + i1 * r2)
    one                     = one  :+ zero

instance (Ring a) => Ring (Complex a) where

instance (Field a) => Invertable (Complex a) where
    reciprocal (a :+ b)            =  let d = a * a + b * b in a / d :+ neg b / d

instance (Field a) => Field (Complex a) where

