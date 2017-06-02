module Main where

{-
stack build && .stack-work/install/x86_64-osx/lts-8.15/8.0.2/bin/algebra
cabal repl
:info Ratio
-}

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd)
import Ratio
import Factorial
import Complex
import Polynomial
--import Modular
import Numeral.Numeral1
import Expression
import Ring
import Field

fr :: Ratio Int
fr = 1 :/ 2

c :: Complex (Ratio Int)
c = (1 :/ 2) :+ (5 :/ 7)
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

assert :: Bool -> a -> a
assert False _ = error "*** assertion failed! ***"
assert _     x = x

test :: (Show a, Eq a) => String -> a -> a -> IO ()
test name expected actual = do
                                putStrLn (name ++ " = " ++ show actual)
                                assert (expected == actual) putStr "" -- oh dear

n0, n1, n2, n3 :: Numeral1 Integer
n0 = numeral 0
n1 = numeral 1
n2 = numeral 2
n3 = numeral 3

p :: Polynomial Integer
p = Term 1 1 (Polynomial.Const 1)

main :: IO ()
main = do
          putStrLn ("factorial1 " ++ show ff ++ " = " ++ show (factorial1 ff))
          putStrLn ("factorial2 " ++ show ff ++ " = " ++ show (factorial2 ff))
          putStrLn ("1 + 2 = " ++ show (n1 + n2))
          putStrLn ("2 * 3 = " ++ show (n2 * n3))
--          putStrLn ("2 ^ 3 = " ++ show (n2 ^ n3))
          putStrLn ("(1 / 2) = " ++ show fr)
          putStrLn ("c = " ++ show c)
          test "c + c" ((1 :/ 1) :+ (10 :/ 7)) (c + c)
          test "c - c" ((0 :/ 1) :+ (0 :/ 1)) (c - c)
          test "c * c" (((-51) :/ 196) :+ (5 :/ 7)) (c * c)
          test "c / c" ((1 :/ 1) :+ (0 :/ 1)) (c / c)
--          test "m + m" (Modular 2) (m + m)
--          putStrLn ("(n1 + n1) = " ++ show (Numeral.n1 Ring.+ Numeral.n1))
--          putStrLn ("(n2 * n3) = " ++ show (Numeral.n2 Prelude.* Numeral.n3))
          putStrLn ("expr = " ++ show expr)
          test "derivative expr" dexpr (derivative expr)
          putStrLn ("eval (ddx expr) = " ++ show (evalExpr 'x' 5 (derivative expr)))
          putStrLn ("p = " ++ show (p * p * p))



