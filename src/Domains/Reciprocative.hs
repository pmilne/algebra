module Domains.Reciprocative where

class Reciprocative a where
  reciprocal :: a -> a

instance Reciprocative Double where
  reciprocal x = 1 / x
