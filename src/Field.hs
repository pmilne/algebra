module Field(module Ring, module Reciprocative, module Field) where

import Prelude hiding ((*))
import Ring
import Reciprocative

infixl 7 /

class (Ring a, Reciprocative a) => Field a where
    (/)      :: a -> a -> a
    x / y     = x * reciprocal y

instance Field Double where
    (/)       = (Prelude./)
