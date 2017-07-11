module Data.Expression.TestInverse where

import           Prelude            hiding (Rational, acos, asin, atan, cos, gcd, log, negate, sin, sqrt, tan, (*), (+),
                                     (-), (/), (^))

import           Domains.Applicable
import           Domains.Exponentiative
import           Domains.Field
import           Domains.Trigonometric

import           Data.Expression
import           Data.Expression.Inverse

import           TestUtil

x :: Expression Double
x = Var "x"

testInverse :: (Eq a, Field a, Exponentiative a, Trigonometric a, Applicable a, Show a) => Expression a -> Expression a -> IO ()
testInverse e d = test ("inverse " ++ show e) d (inverse e)

--inv2 :: (a -> a) -> (a -> a)
--inv2 f = case
half :: Expression Double
half = Const 0.5

run :: IO ()
run
--          putStrLn ("derivative^2 (tan (tan x1)) = " ++ show (derivative (derivative (tan (tan x1)))))))
 = do
  testInverse (x ~> x / two) (x ~> two * x)
  testInverse (x ~> two / x) (x ~> reciprocal (half * x))
  testInverse (x ~> neg (reciprocal x)) (x ~> reciprocal (neg x))
  testInverse (x ~> sin x) (x ~> asin x)
  testInverse (x ~> sin (cos x)) (x ~> acos (asin x))
  testInverse (x ~> sin (cos (tan x))) (x ~> atan (acos (asin x)))
  testInverse (x ~> sin (x + one)) (x ~> asin x - one)
  testInverse (x ~> sin (one + x)) (x ~> neg one + asin x)
  testInverse (x ~> sin (x * two)) (x ~> half * asin x)
  testInverse (x ~> sin (two * x)) (x ~> half * asin x)
  testInverse (x ~> two ^ x) (x ~> log two x)
  testInverse (x ~> two ^ sin x) (x ~> asin (log two x))
  testInverse (x ~> x ^ two) (x ~> x ^ reciprocal two)
  testInverse (x ~> sin x ^ two) (x ~> asin (x ^ reciprocal two))
