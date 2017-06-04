module Numeral.Numeral1 where

import Ring

-- A Church Numeral type
newtype Numeral1 t = Numeral1 ((t -> t) -> (t -> t))

numeral :: Integer -> Numeral1 t
numeral i = Numeral1 (church i)

church :: Integer -> ((t -> t) -> (t -> t))
church 0 = \ _ x -> x
church n = \ f x -> f (church (n Prelude.- 1) f x)

instance (Show s, Num s) => Show (Numeral1 s) where
   show (Numeral1 n1) = show (n1 (Prelude.+1) 0)

instance  (Additive a) => Additive (Numeral1 a) where
    (Numeral1 n1) + (Numeral1 n2) = Numeral1 (\f x -> (n1 f (n2 f x)))
    zero                          = Numeral1 (\_ x -> x)

instance  (Ring a) => Ring (Numeral1 a) where
    (Numeral1 n1) * (Numeral1 n2) = Numeral1 (n1 . n2)
    negate (Numeral1 _)           = undefined
    one                           = Numeral1 id

