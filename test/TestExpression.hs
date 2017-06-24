module TestExpression where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd, Rational, sqrt, sin, cos, tan, asin, acos, atan, log)

import Field
import Exponentiative
import Trigonometric
import Applicable

import Rational
import Expression

import TestUtil

import qualified Expression.TestDerivative as TestDerivative

import Hack

y :: Expression (Rational Integer)
y = Var "y"

pc :: (Euclidean a, Ring a) => a -> Expression (Rational a)
pc xx = Const (rational1 xx)

xp1 :: Expression (Rational Integer)
xp1 = y + pc 1

x :: Expression Double
x = Var "x"

testInverse :: (Eq a, Field a, Exponentiative a, Applicable a, Show a) => Expression a -> Expression a -> IO ()
testInverse e d = test ("inverse " ++ show e) d (inverse e)

--inv2 :: (a -> a) -> (a -> a)
--inv2 f = case

half :: Expression Double
half = Const 0.5

run :: IO ()
run = do
          putStrLn ("sin (1) = " ++ show (eval1 "x" 1.0 (sin x)))

          TestDerivative.run

--          putStrLn ("derivative^2 (tan (tan x1)) = " ++ show (derivative (derivative (tan (tan x1)))))))

          testInverse (x ~> x / two) (x ~> two * x)
          testInverse (x ~> two / x) (x ~> inv (half * x))

          testInverse (x ~> neg (inv x)) (x ~> inv (neg x))
          testInverse (x ~> sin x) (x ~> asin x)
          testInverse (x ~> sin (cos x)) (x ~> acos (asin x))
          testInverse (x ~> sin (cos (tan x))) (x ~> atan (acos (asin x)))

          testInverse (x ~> sin (x + one)) (x ~> asin x - one)
          testInverse (x ~> sin (one + x)) (x ~> neg one + asin x)

          testInverse (x ~> sin (x * two)) (x ~> half * asin x)
          testInverse (x ~> sin (two * x)) (x ~> half * asin x)

          testInverse (x ~> two^x) (x ~> log two x)
          testInverse (x ~> two ^ sin x) (x ~> asin (log two x))
          testInverse (x ~> x^two) (x ~> x ^ inv two)
          testInverse (x ~> sin x ^ two) (x ~> asin (x ^ inv two))

