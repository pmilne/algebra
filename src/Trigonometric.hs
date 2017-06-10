module Trigonometric where

class Trigonometric a where
    sin      :: a -> a
    cos      :: a -> a
    tan      :: a -> a
    asin     :: a -> a
    acos     :: a -> a
    atan     :: a -> a

instance Trigonometric Double where
    sin       = Prelude.sin
    cos       = Prelude.cos
    tan       = Prelude.tan
    asin      = Prelude.asin
    acos      = Prelude.acos
    atan      = Prelude.atan

