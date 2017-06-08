-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE FlexibleInstances #-}

module Additive where

infixl 6 +

class Additive a where
    (+)      :: a -> a -> a
    zero     :: a

{-
instance Additive String where
    (+)       = (Prelude.++)
    zero      = ""
-}

instance Additive Int where
    (+)       = (Prelude.+)
    zero      = 0

instance Additive Integer where
    (+)       = (Prelude.+)
    zero      = 0

instance Additive Double where
    (+)       = (Prelude.+)
    zero      = 0

