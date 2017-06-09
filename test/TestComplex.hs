module TestComplex where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd, Rational)
import Ring
import Complex
import Rational
import Field
import TestUtil

c :: Complex (Rational Int)
c = Rational 1 2 :+ Rational 5 7
--c :: Complex Double
--c = 0.5 :+ 0.25


testPoly :: IO ()
testPoly = do
          putStrLn ("c = " ++ show c)
          test "c + c" (Rational 1 1 :+ Rational 10 7) (c + c)
          test "c - c" (Rational 0 1 :+ Rational 0 1) (c - c)
          test "c * c" (Rational (-51) 196 :+ Rational 5 7) (c * c)
          test "c / c" (Rational 1 1 :+ Rational 0 1) (c / c)

