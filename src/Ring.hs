module Ring where

infixl 6 +, -
infixl 7 *
infixr 8 ^

class Ring a where
    (+)      :: a -> a -> a
    (-)      :: a -> a -> a
    (*)      :: a -> a -> a
    (^)      :: a -> a -> a
    zero     :: a
    one      :: a
    negate   :: a -> a
--    promote  :: b -> a
    x - y               = x Ring.+ Ring.negate y
    negate x            = Ring.zero Ring.- x

instance Ring Int where
    (+)       = (Prelude.+)
    (*)       = (Prelude.*)
    (^)       = (Prelude.^)
    zero      = 0
    one       = 1
    negate    = Prelude.negate
