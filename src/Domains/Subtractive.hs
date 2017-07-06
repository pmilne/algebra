module Domains.Subtractive
  ( module Domains.Additive
  , module Domains.Negatable
  , module Domains.Subtractive
  ) where

import           Domains.Additive
import           Domains.Negatable
import           Prelude           hiding (negate, (*), (+), (-), (/), (^))

infixl 6 -

class (Additive a, Negatable a) =>
      Subtractive a where
  (-) :: a -> a -> a
  x - y = x + neg y

instance Subtractive Int

instance Subtractive Integer

instance Subtractive Double
