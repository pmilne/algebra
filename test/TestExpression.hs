module TestExpression where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd, Rational, sqrt, sin, cos, tan, asin, acos, atan)

import Field
import Exponentiative
import Trigonometric

import Rational
import Expression

import TestUtil

instance Exponentiative (Rational a) where
  _ ^ _ = undefined
  ln _  = undefined
  exp _ = undefined
  sqrt  = undefined
  two   = undefined

x :: Expression (Rational Integer)
x = Var "x"

pc :: (Euclidean a, Ring a) => a -> Expression (Rational a)
pc xx = Const (rational1 xx)

xp1 :: Expression (Rational Integer)
xp1 = x + pc 1

x1 :: Expression Double
x1 = Var "x1"

testDerivative :: (Eq a, Field a, Exponentiative a, Show a) => Expression a -> Expression a -> IO ()
testDerivative e d = test ("derivative " ++ show e) d (derivative e)

testInverse :: (Eq a, Field a, Exponentiative a, Applicable a, Show a) => Expression a -> Expression a -> IO ()
testInverse e d = test ("inverse " ++ show e) d (inverse e)

--inv2 :: (a -> a) -> (a -> a)
--inv2 f = case

run :: IO ()
run = do
          testDerivative (x + pc 1) (pc 1)
          testDerivative (pc 3 * x ^ pc 2) (pc 6 * x)
          testDerivative (x ^ x) (x ^ x * (pc 1 + ln x))
          testDerivative (ln (ln x)) (pc 1 / (x * ln x))

          testDerivative (sin x1) (cos x1)
          testDerivative (x1 + sin x1) (one + cos x1)
          testDerivative (sin (sin x1))  (cos x1 * cos (sin x1))
          testDerivative (tan (tan x1))  (one/((cos x1 ^ two)*(cos (tan x1) ^ two)))
          testDerivative (asin x1)   (one / sqrt (one + neg (x1 ^ two)))
          testDerivative (derivative (x1 + sin x1)) (neg (sin x1))

--          putStrLn ("derivative^2 (tan (tan x1)) = " ++ show (derivative (derivative (tan (tan x1)))))

          putStrLn ("sin (1) = " ++ show (eval "x1" 1.0 (sin x1)))

          testInverse (neg (inv x1)) (inv (neg x1))
          testInverse (sin x1) (asin x1)
          testInverse (sin (cos x1)) (acos (asin x1))
          testInverse (sin (cos (tan x1))) (atan (acos (asin x1)))
