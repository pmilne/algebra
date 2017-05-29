module Main where

{-
stack build && .stack-work/install/x86_64-osx/lts-8.15/8.0.2/bin/algebra
-}

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd)
import Ratio
import Factorial
import Complex
import Numeral
import Expression
import Ring

fr :: Ratio Int
fr = 1 :/ 2

g :: Complex (Ratio Int)
g = (1 :/ 2) :+ (5 :/ 7)

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
dexpr = Pow var var * (Const 1.0 + Log var)

assert :: Bool -> a -> a
assert False _ = error "*** assertion failed! ***"
assert _     x = x

test :: (Show a, Eq a) => String -> a -> a -> IO ()
test name expected actual = assert (expected == actual) putStrLn (name ++ " = " ++ show actual)

n0, n1, n2, n3 :: Numeral Integer
n0 = numeral 0
n1 = numeral 1
n2 = numeral 2
n3 = numeral 3

main :: IO ()
main = do
          putStrLn ("factorial1 " ++ show ff ++ " = " ++ show (factorial1 ff))
          putStrLn ("factorial2 " ++ show ff ++ " = " ++ show (factorial2 ff))
          putStrLn ("1 + 2 = " ++ show (n1 + n2))
          putStrLn ("2 * 3 = " ++ show (n2 * n3))
--          putStrLn ("2 ^ 3 = " ++ show (n2 ^ n3))
          putStrLn ("(1 / 2) = " ++ show fr)
          putStrLn ("g = " ++ show g)
          test "g + g" ((1 :/ 1) :+ (10 :/ 7)) (g + g)
          test "g * g" (((-51) :/ 196) :+ (5 :/ 7)) (g * g)
--          putStrLn ("(n1 + n1) = " ++ show (Numeral.n1 Ring.+ Numeral.n1))
--          putStrLn ("(n2 * n3) = " ++ show (Numeral.n2 Prelude.* Numeral.n3))
          putStrLn ("expr = " ++ show expr)
          putStrLn ("derivative expr = " ++ show (derivative expr))
          test "ddx expr" dexpr (ddx expr)
          putStrLn ("eval (ddx expr) = " ++ show (evalExpr 'x' 5 (ddx expr)))


