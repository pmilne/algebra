module Data.TestComplex where

import           Domains.Field
import           Domains.Ring
import           Prelude       hiding (Rational, gcd, negate, (*), (+), (-), (/), (^))

import           Data.Complex
import           Data.Rational

import           TestUtil

c :: Complex (Rational Int)
c = Rational 1 2 :+ Rational 5 7

--c :: Complex Double
--c = 0.5 :+ 0.25
run :: IO ()
run = do
  putStrLn ("c = " ++ show c)
  test "c + c" (Rational 1 1 :+ Rational 10 7) (c + c)
  test "c - c" (Rational 0 1 :+ Rational 0 1) (c - c)
  test "c * c" (Rational (-51) 196 :+ Rational 5 7) (c * c)
  test "c / c" (Rational 1 1 :+ Rational 0 1) (c / c)
