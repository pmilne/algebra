module Main where

{-
stack build && .stack-work/install/x86_64-osx/lts-8.15/8.0.2/bin/algebra
cabal repl
:info Ratio
-}

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd, Rational)
import Rational
import Factorial
import Complex
import Polynomial
import Modular
import Numeral.Numeral1
import Expression
import Field
import Euclidean
import Collins
import TestPolynomial
import TestUtil

fr :: Rational Int
fr = Rational 1 2

c :: Complex (Rational Int)
c = Rational 1 2 :+ Rational 5 7
--c :: Complex Double
--c = 0.5 :+ 0.25

ff :: Int
--ff = 2 ^ 27
ff = 6

var :: Expression Double
var = Var 'x'

expr :: Expression Double
--expr = Const 3 * Pow var (Const 2) -- 3x^2
expr = Pow var var -- x ^ x

dexpr :: Expression Double
--dexpr = Const 6 * var -- 6x
dexpr = Pow var var * (Expression.Const 1.0 + Log var)

n0, n1, n2, n3 :: Numeral1 Integer
n0 = numeral 0
n1 = numeral 1
n2 = numeral 2
n3 = numeral 3

m0, m1 :: Modular Int
m0 = Modular 3
m1 = Modular 2

{-
pp0 :: Polynomial (Polynomial (Polynomial (Polynomial Integer)))
pp0 = zero

foo :: (Multiplicative a) => a -> a
foo x = one

bar :: (Multiplicative a) => Polynomial a -> a
bar x = one

baz :: (Multiplicative a) => Polynomial (Polynomial a) -> a
baz x = one
-}

main :: IO ()
main = do
          putStrLn ("factorial1 " ++ show ff ++ " = " ++ show (factorial1 ff))
          putStrLn ("factorial2 " ++ show ff ++ " = " ++ show (factorial2 ff))
          putStrLn ("1 + 2 = " ++ show (n1 + n2))
          putStrLn ("2 * 3 = " ++ show (n2 * n3))
--          putStrLn ("2 ^ 3 = " ++ show (n2 ^ n3))
          putStrLn ("(1 / 2) = " ++ show fr)
          putStrLn ("c = " ++ show c)
          test "c + c" (Rational 1 1 :+ Rational 10 7) (c + c)
          test "c - c" (Rational 0 1 :+ Rational 0 1) (c - c)
          test "c * c" (Rational (-51) 196 :+ Rational 5 7) (c * c)
          test "c / c" (Rational 1 1 :+ Rational 0 1) (c / c)
          putStrLn ("2 * 3 mod 4 = " ++ show (m0 * m1))
          test "m + m" (Modular 2) (m + m)
          putStrLn ("expr = " ++ show expr)
          test "derivative expr" dexpr (derivative expr)
          putStrLn ("eval (ddx expr) = " ++ show (evalExpr 'x' 5 (derivative expr)))
          putStrLn ("expr = " ++ show expr)
{-
          putStrLn ("zero = " ++ show (foo (undefined :: Polynomial (Polynomial (Polynomial (Polynomial Integer))))))
          putStrLn ("zero = " ++ show (foo (undefined :: Polynomial (Polynomial Integer))))
          putStrLn ("zero = " ++ show (bar (undefined :: Polynomial (Polynomial Integer))))
          putStrLn ("zero = " ++ show (foo pp0))
          putStrLn ("zero = " ++ show (bar pp0))
          putStrLn ("zero = " ++ show (baz pp0))
-}
          TestPolynomial.testPoly





