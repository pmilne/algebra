module Numeral.TestNumeral1 where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd, Rational)
import Domains.Ring

import Numeral.Numeral1

import TestUtil

n0, n1, n2, n3 :: Numeral1 Integer
n0 = numeral 0
n1 = numeral 1
n2 = numeral 2
n3 = numeral 3


run :: IO ()
run = do
          putStrLn ("1 + 2 = " ++ show (n1 + n2))
          putStrLn ("2 * 3 = " ++ show (n2 * n3))
--          putStrLn ("2 ^ 3 = " ++ show (n2 ^ n3))
--          putStrLn ("(n1 + n1) = " ++ show (Numeral.n1 Ring.+ Numeral.n1))
--          putStrLn ("(n2 * n3) = " ++ show (Numeral.n2 Prelude.* Numeral.n3))

