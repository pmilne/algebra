module TestRational where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd, Rational)
import Ring
import Complex
import Rational
import Field
import TestUtil

fr :: Rational Int
fr = Rational 1 2


run :: IO ()
run = do
          putStrLn ("(1 / 2) = " ++ show fr)
