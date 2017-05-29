module Numeral where

import Ring
import Numeral2

-- A Church Numeral type
newtype Numeral t = Numeral ((t -> t) -> (t -> t))

numeral :: Integer -> Numeral t
numeral i = Numeral (Numeral2.church i)

instance (Show s, Num s) => Show (Numeral s) where
   show (Numeral n1) = show (n1 (Prelude.+1) 0)

instance  (Ring a) => Ring (Numeral a) where
    (Numeral n1) + (Numeral n2) = Numeral (\f x -> (n1 f (n2 f x)))
    (Numeral n1) * (Numeral n2) = Numeral (n1 . n2)
    negate (Numeral _)          = undefined
    zero                        = Numeral (\_ x -> x)
    one                         = Numeral id

