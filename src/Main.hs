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
import Additive
import Ring
import Field
import Collins

fr :: Ratio Int
fr = Ratio 1 2

c :: Complex (Ratio Int)
c = Ratio 1 2 :+ Ratio 5 7
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

x, x2, x3, x4, x5, x6, x7 :: Polynomial Integer
x = Term 1 1 (Polynomial.Const 0)
x2 = x * x;
x3 = x2 * x
x4 = x3 * x
x5 = x4 * x
x6 = x5 * x
x7 = x6 * x
x8 = x7 * x

p :: Polynomial Integer
p = Term 1 1 (Polynomial.Const 1)

pc :: Integer -> Polynomial Integer
pc = promote

main :: IO ()
main = do
          putStrLn ("factorial1 " ++ show ff ++ " = " ++ show (factorial1 ff))
          putStrLn ("factorial2 " ++ show ff ++ " = " ++ show (factorial2 ff))
          putStrLn ("1 + 2 = " ++ show (n1 + n2))
          putStrLn ("2 * 3 = " ++ show (n2 * n3))
--          putStrLn ("2 ^ 3 = " ++ show (n2 ^ n3))
          putStrLn ("(1 / 2) = " ++ show fr)
          putStrLn ("c = " ++ show c)
          test "c + c" (Ratio 1 1 :+ Ratio 10 7) (c + c)
          test "c - c" (Ratio 0 1 :+ Ratio 0 1) (c - c)
          test "c * c" (Ratio (-51) 196 :+ Ratio 5 7) (c * c)
          test "c / c" (Ratio 1 1 :+ Ratio 0 1) (c / c)
--          test "m + m" (Modular 2) (m + m)
--          putStrLn ("(n1 + n1) = " ++ show (Numeral.n1 Ring.+ Numeral.n1))
--          putStrLn ("(n2 * n3) = " ++ show (Numeral.n2 Prelude.* Numeral.n3))
--          print (Collins.gcd (pc 0) (pc 1))
--          print (Collins.gcd (pc 1) (pc 0))
--          print (Collins.gcd (pc 3) (pc 7))
--          print (Collins.gcd (pc 3 * x + pc 1) (pc 7))
--          print (Collins.resultant (x2 - pc 1) (x - pc 1))
--          print (Collins.resultant (x2 - pc 1) (x2 - pc 2 * x + pc 1))
--          print (Collins.gcd (x2 - pc 1) (x2 - pc 2 * x + pc 1))
          print (Collins.gcd (pc 3 * x2 + pc 1) (pc 5 * x4 + x2 + pc 4))
          print (Collins.gcd (x8 + x6 - pc 3 * x4 - pc 3 * x3 + pc 8 * x2 + pc 2 * x - pc 5) (pc 3 * x6 + pc 5 * x4 - pc 4 * x2 - pc 9 * x + pc 21))
          putStrLn ("expr = " ++ show expr)
          test "derivative expr" dexpr (derivative expr)
          putStrLn ("eval (ddx expr) = " ++ show (evalExpr 'x' 5 (derivative expr)))
          putStrLn ("p = " ++ show (p * p * p))





