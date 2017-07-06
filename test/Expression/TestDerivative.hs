module Expression.TestDerivative where

import           Expression.Derivative
import           Prelude                hiding (Rational, acos, asin, atan, cos, gcd, log, negate, sin, sqrt, tan, (*),
                                         (+), (-), (/), (^))
import           Rational

import           Domains.Applicable
import           Domains.Exponentiative
import           Domains.Field
import           Domains.Trigonometric

import           Hack

import           Expression

import           TestUtil

y :: Expression (Rational Integer)
y = Var "y"

pc :: (Euclidean a, Ring a) => a -> Expression (Rational a)
pc xx = Const (rational1 xx)

xp1 :: Expression (Rational Integer)
xp1 = y + pc 1

x :: Expression Double
x = Var "x"

testDerivative ::
     (Eq a, Field a, Exponentiative a, Trigonometric a, Applicable a, Show a) => Expression a -> Expression a -> IO ()
testDerivative e d = test ("derivative " ++ show e) d (derivative e)

--inv2 :: (a -> a) -> (a -> a)
--inv2 f = case
half :: Expression Double
half = Const 0.5

run :: IO ()
run = do
  testDerivative (y ~> (y + pc 1)) (y ~> pc 1)
  testDerivative (y ~> (pc 3 * y ^ pc 2)) (y ~> pc 6 * y)
  testDerivative (y ~> y ^ y) (y ~> y ^ y * (pc 1 + ln y))
  testDerivative (y ~> ln (ln y)) (y ~> (pc 1 / (y * ln y)))
  testDerivative (x ~> sin x) (x ~> cos x)
  testDerivative (x ~> (x + sin x)) (x ~> (one + cos x))
  testDerivative (x ~> sin (sin x)) (x ~> (cos x * cos (sin x)))
  testDerivative (x ~> tan (tan x)) (x ~> (one / ((cos x ^ two) * (cos (tan x) ^ two))))
  testDerivative (x ~> asin x) (x ~> (one / sqrt (one + neg (x ^ two))))
  testDerivative (x ~> tan x) (x ~> (one / cos x ^ two))
  testDerivative (x ~> (sin x / cos x)) (x ~> (((sin x ^ two) / (cos x ^ two)) + one))
--          testDD (y ~>  (derivative (x + sin x))) (neg (sin x)))
--          putStrLn ("derivative^2 (tan (tan x1)) = " ++ show (derivative (derivative (tan (tan x1)))))))
