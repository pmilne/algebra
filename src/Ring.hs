module Ring(module Additive, module Subtractive, module Multiplicative, module Ring) where

import Additive
import Subtractive
import Multiplicative

-- https://en.wikipedia.org/wiki/Ring_(mathematics)
class (Additive a, Subtractive a, Multiplicative a) => Ring a where

instance Ring Int where

instance Ring Integer where

instance Ring Double where


