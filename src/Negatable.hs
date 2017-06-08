module Negatable where

class Negatable a where
    neg   :: a -> a

instance Negatable Int where
    neg    = Prelude.negate

instance Negatable Integer where
    neg    = Prelude.negate

instance Negatable Double where
    neg    = Prelude.negate
