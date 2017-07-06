module Domains.Field
  ( module Domains.Ring
  , module Domains.Reciprocative
  , module Domains.Field
  ) where

import           Domains.Reciprocative
import           Domains.Ring
import           Prelude               hiding ((*))

infixl 7 /

class (Ring a, Reciprocative a) =>
      Field a where
  (/) :: a -> a -> a
  x / y = x * reciprocal y

instance Field Double where
  (/) = (Prelude./)
