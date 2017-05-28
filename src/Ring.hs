{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Ring where

infixl 6 +, -
infixl 7 *
infixr 8 ^, /

class Ring a where
    (+)      :: a -> a -> a
    (-)      :: a -> a -> a
    (*)      :: a -> a -> a
    (/)      :: a -> a -> a
    (^)      :: a -> a -> a
    zero     :: a
    one      :: a
    negate   :: a -> a
    inv      :: a -> a
--    promote  :: b -> a
    x - y               = x Ring.+ Ring.negate y
    x / y               = x Ring.* Ring.inv y
    negate x            = Ring.zero Ring.- x
    inv x               = Ring.one Ring./ x
    quo      :: a -> a -> a
    rem      :: a -> a -> a
    gcd      :: a -> a -> a
    gcd       = undefined
    quo       = undefined
    rem       = undefined

instance Ring Int where
    (+)       = (Prelude.+)
    (*)       = (Prelude.*)
    (^)       = (Prelude.^)
    (/)       = undefined
    zero      = 0
    one       = 1
    negate    = Prelude.negate
    quo       = Prelude.quot
    rem       = Prelude.rem
    inv       = undefined
    gcd       = Prelude.gcd

