module Domains.Multiplicative where

infixl 7 *

class Multiplicative a where
  (*) :: a -> a -> a
  one :: a

instance Multiplicative Int where
  (*) = (Prelude.*)
  one = 1

instance Multiplicative Integer where
  (*) = (Prelude.*)
  one = 1

instance Multiplicative Double where
  (*) = (Prelude.*)
  one = 1
