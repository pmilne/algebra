module Field where

import Prelude hiding ((*), (/))
import Ring

infixr 7 /

class Ring a => Field a where
    (/)      :: a -> a -> a
    inv      :: a -> a
    x / y               = x * inv y
    inv x               = one / x


