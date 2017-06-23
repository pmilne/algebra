module TestExpression where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd, Rational, sqrt, sin, cos, tan, asin, acos, atan, log)

import Field
import Exponentiative
import Trigonometric
import Applicable

import Rational
import Expression

import TestUtil

instance Exponentiative (Rational a) where
  _ ^ _   = undefined
  log _ _ = undefined
  ln _    = undefined
  exp _   = undefined
  sqrt    = undefined
  two     = undefined

y :: Expression (Rational Integer)
y = Var "y"

pc :: (Euclidean a, Ring a) => a -> Expression (Rational a)
pc xx = Const (rational1 xx)

xp1 :: Expression (Rational Integer)
xp1 = y + pc 1

x :: Expression Double
x = Var "x"

testDerivative :: (Eq a, Field a, Exponentiative a, Show a) => Expression a -> Expression a -> IO ()
testDerivative e d = test ("derivative " ++ show e) d (derivative e)

testInverse :: (Eq a, Field a, Exponentiative a, Applicable a, Show a) => Expression a -> Expression a -> IO ()
testInverse e d = test ("inverse " ++ show e) d (inverse e)

--inv2 :: (a -> a) -> (a -> a)
--inv2 f = case

half :: Expression Double
half = Const 0.5

run :: IO ()
run = do
          putStrLn ("sin (1) = " ++ show (eval1 "x" 1.0 (sin x)))

          testDerivative (y ~> (y + pc 1)) (y ~> (pc 1))
          testDerivative (y ~> (pc 3 * y ^ pc 2)) (y ~> (pc 6 * y))
          testDerivative (y ~> (y ^ y)) (y ~> (y ^ y * (pc 1 + ln y)))
          testDerivative (y ~> (ln (ln y))) (y ~> (pc 1 / (y * ln y)))

          testDerivative (x ~> (sin x)) (x ~> (cos x))
          testDerivative (x ~> (x + sin x)) (x ~> (one + cos x))
          testDerivative (x ~> (sin (sin x))) (x ~> (cos x * cos (sin x)))
          testDerivative (x ~> (tan (tan x))) (x ~> (one/((cos x ^ two)*(cos (tan x) ^ two))))
          testDerivative (x ~> (asin x)) (x ~> (one / sqrt (one + neg (x ^ two))))
          testDerivative (x ~> (tan x)) (x ~> (one / cos x^two))
          testDerivative (x ~> (sin x / cos x)) (x ~> (((sin x ^ two) /(cos x ^ two)) + one))
--          testDD (y ~>  (derivative (x + sin x))) (neg (sin x)))

--          putStrLn ("derivative^2 (tan (tan x1)) = " ++ show (derivative (derivative (tan (tan x1)))))))

          testInverse (x / two) (two * x)
          testInverse (two / x) (inv (half * x))

          testInverse (neg (inv x)) (inv (neg x))
          testInverse (sin x) (asin x)
          testInverse (sin (cos x)) (acos (asin x))
          testInverse (sin (cos (tan x))) (atan (acos (asin x)))

          testInverse (sin (x + one)) (asin x - one)
          testInverse (sin (one + x)) (neg one + asin x)

          testInverse (sin (x * two)) (half * asin x)
          testInverse (sin (two * x)) (half * asin x)

          testInverse (two^x) (log two x)
          testInverse (two ^ sin x) (asin (log two x))
          testInverse (x^two) (x ^ inv two)
          testInverse (sin x ^ two) (asin (x ^ inv two))

