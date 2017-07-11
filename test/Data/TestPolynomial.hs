module Data.TestPolynomial where

import           Prelude           hiding (Rational, gcd, negate, (*), (+), (-), (/), (^))

import           Domains.Euclidean
import           Domains.Ring

import           Data.Polynomial
import           Data.Rational

import           Collins
import           TestUtil

x, x2, x3, x4, x5, x6, x7, x8 :: Polynomial Integer
x = Term 1 1 (Data.Polynomial.Const 0)

x2 = x * x

x3 = x2 * x

x4 = x3 * x

x5 = x4 * x

x6 = x5 * x

x7 = x6 * x

x8 = x7 * x

p :: Polynomial Integer
p = Term 1 1 (Data.Polynomial.Const 1)

pc :: Integer -> Polynomial Integer
pc = promote

run :: IO ()
run
--          print (Collins.gcd (pc 0) (pc 1))
--          print (Collins.gcd (pc 1) (pc 0))
--          print (Collins.gcd (pc 3) (pc 7))
--          print (Collins.gcd (pc 3 * x + pc 1) (pc 7))
 = do
  test "gcd" (x - pc 1) (gcd (x2 - pc 1) (x - pc 1))
  putStrLn ("gcd " ++ show (gcd (pc 6 * (x2 - pc 1)) (pc 4 * (x - pc 1))))
  test "ratio " (rational (pc 3 * x + pc 3) (pc 2)) (rational (pc 6 * (x2 - pc 1)) (pc 4 * (x - pc 1)))
  test "gcd" (x - one) (gcd (x2 - pc 1) (x2 - pc 2 * x + pc 1))
  test "ratio " (rational (x + one) (x - one)) (rational (x2 - pc 1) (x2 - pc 2 * x + pc 1))
  test "gcd" one (gcd (pc 3 * x2 + pc 1) (pc 5 * x4 + x2 + pc 4))
  test
    "subresultant"
    260708
    (Collins.resultant
       (x8 + x6 - pc 3 * x4 - pc 3 * x3 + pc 8 * x2 + pc 2 * x - pc 5)
       (pc 3 * x6 + pc 5 * x4 - pc 4 * x2 - pc 9 * x + pc 21))
  putStrLn ("p = " ++ show (p * p * p))
