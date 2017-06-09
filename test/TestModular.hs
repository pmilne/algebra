module TestModular where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd, Rational)

import Ring

import Modular

import TestUtil

m0, m1 :: Modular Int
m0 = Modular 3
m1 = Modular 2

run :: IO ()
run = do
          putStrLn ("2 * 3 mod 4 = " ++ show (m0 * m1))
          test "m + m" (Modular 2) (m + m)
