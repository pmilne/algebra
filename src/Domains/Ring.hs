module Domains.Ring
  ( module Domains.Additive
  , module Domains.Subtractive
  , module Domains.Multiplicative
  , module Domains.Ring
  ) where

import           Domains.Additive
import           Domains.Multiplicative
import           Domains.Subtractive

-- https://en.wikipedia.org/wiki/Ring_(mathematics)
class (Additive a, Subtractive a, Multiplicative a) =>
      Ring a

instance Ring Int

instance Ring Integer

instance Ring Double
