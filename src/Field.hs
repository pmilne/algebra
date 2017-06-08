module Field(module Ring, module Invertable, module Field) where

import Prelude hiding ((*))
import Ring
import Invertable

infixl 7 /

class (Ring a, Invertable a) => Field a where
    (/)      :: a -> a -> a
    x / y     = x * inv y
--    inv x     = one Field./ x

instance Field Double where
    (/)       = (Prelude./)
