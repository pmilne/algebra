module Euclidean where

class Euclidean a where
--    quo       :: a -> a -> a
--    rem       :: a -> a -> a
--    gcd       :: a -> a -> a
    canonical :: a -> a -> (a -> a -> b) -> b

instance Euclidean Int where
--    quo           = Prelude.quot
--    rem           = Prelude.rem
--    gcd           = Prelude.gcd
    canonical n d = let g = Prelude.signum d * Prelude.gcd n d in \f -> f (Prelude.quot n g) (Prelude.quot d g)

