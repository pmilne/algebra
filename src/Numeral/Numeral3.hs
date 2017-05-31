{-# LANGUAGE ExistentialQuantification #-}

module Numeral3 where

import Ring

data Numeral3 = forall a. Numeral3 ((a -> a) -> (a -> a))

instance Ring Numeral3 where
--    Numeral3 n1 + Numeral3 n2 = Numeral3 (\f x -> (n1 f (n2 f x)))
--    Numeral3 n1 * Numeral3 n2 = Numeral3 (\f -> (n1 (n2 f)))