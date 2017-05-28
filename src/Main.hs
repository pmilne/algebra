module Main where

{-
stack build && .stack-work/install/x86_64-osx/lts-8.15/8.0.2/bin/woot
-}

import Ratio
--import Data.Int
import Factorial
import Complex
import Numeral
import Expr
import Ring

g :: Complex (Ratio Int)
g = (1 :/ 2) :+ (5 :/ 7)

ff :: Int
--ff = 2 ^ 27
ff = 6

expr :: Expr Int
expr = Const 3 Ring.* (Var 'x' :^: Const 2) -- 3x^2

dexpr :: Expr Int
dexpr = Const 6 Ring.* Var 'x' -- 6x

assert :: Bool -> a -> a
assert False x = error "*** assertion failed! ***"
assert _     x = x

test :: (Show a, Eq a) => [Char] -> a -> a -> IO ()
test name expected actual = assert (expected == actual) putStrLn (name ++ " = " ++ show actual)

main :: IO ()
main = do
          putStrLn ("factorial1 " ++ show ff ++ " = " ++ show (factorial1 ff))
          putStrLn ("factorial2 " ++ show ff ++ " = " ++ show (factorial2 ff))
          putStrLn ("three = " ++ show (unchurch three))
          putStrLn ("eight = " ++ show (unchurch eight))
          putStrLn ("(plus1 two three) = " ++ show (unchurch (plus1 two three)))
          putStrLn ("(times1 two three) = " ++ show (unchurch (times1 two three)))
          putStrLn ("(expt1 two three) = " ++ show (unchurch (expt1 two three)))
          putStrLn ("(2 % 3) = " ++ show ((2::Int) :/ (3::Int)))
          putStrLn ("(2 % 3 :+ 5 % 7) = " ++ show (((2::Int) :/ (3::Int)) :+ ((5::Int) :/ (7::Int))))
          putStrLn ("(1 % 2 :+ 5 % 7) = " ++ show (g Ring.+ g))
          putStrLn ("(1 % 2 :+ 5 % 7) ^ 2 = " ++ show (g Ring.* g))
--          putStrLn ("(n1 + n1) = " ++ show (Numeral.n1 Ring.+ Numeral.n1))
--          putStrLn ("(n2 * n3) = " ++ show (Numeral.n2 Prelude.* Numeral.n3))
          putStrLn ("expr = " ++ show expr)
          putStrLn ("derivative expr = " ++ show (derivative expr))
          test "ddx expr" dexpr (ddx expr)
          putStrLn ("eval (ddx expr) = " ++ show (evalExpr 'x' 5 (ddx expr)))


