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
import TestUtil

import TestComplex
import TestPolynomial

fr :: Rational Int
fr = Rational 1 2

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
          putStrLn ("2 * 3 mod 4 = " ++ show (m0 * m1))
          test "m + m" (Modular 2) (m + m)
          putStrLn ("expr = " ++ show expr)
          test "derivative expr" dexpr (derivative expr)
          putStrLn ("eval (ddx expr) = " ++ show (evalExpr 'x' 5 (derivative expr)))
          putStrLn ("expr = " ++ show expr)
--          putStrLn ("(n1 + n1) = " ++ show (Numeral.n1 Ring.+ Numeral.n1))
--          putStrLn ("(n2 * n3) = " ++ show (Numeral.n2 Prelude.* Numeral.n3))
{-
          putStrLn ("zero = " ++ show (foo (undefined :: Polynomial (Polynomial (Polynomial (Polynomial Integer))))))
          putStrLn ("zero = " ++ show (foo (undefined :: Polynomial (Polynomial Integer))))
          putStrLn ("zero = " ++ show (bar (undefined :: Polynomial (Polynomial Integer))))
          putStrLn ("zero = " ++ show (foo pp0))
          putStrLn ("zero = " ++ show (bar pp0))
          putStrLn ("zero = " ++ show (baz pp0))
-}
          TestComplex.testPoly
          TestPolynomial.testPoly





