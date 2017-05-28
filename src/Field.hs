{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Field where

import Ring

--import Ratio

infixr 8 /

class Ring a => Field a where
--    (*)      :: a -> a -> a
    (/)      :: a -> a -> a
    inv      :: a -> a
    x / y               = x Ring.* inv y
    inv x               = one Field./ x
--    one      :: a

instance Field Int where
    (/)       = undefined -- (Ratio.:/)
    inv       = undefined

