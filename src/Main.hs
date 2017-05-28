module Main where

{-
stack build && .stack-work/install/x86_64-osx/lts-8.15/8.0.2/bin/woot
-}

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd)
import Ratio
--import Data.Int
import Factorial
import Complex
import Numeral
import Expr
import Ring

fr :: Ratio Int
fr = 1 / 2

g :: Complex (Ratio Int)
g = (1 / 2) :+ (5 / 7)

ff :: Int
--ff = 2 ^ 27
ff = 6

expr :: Expr Int
expr = Const 3 Ring.* (Var 'x' :^: Const 2) -- 3x^2

dexpr :: Expr Int
dexpr = Const 6 Ring.* Var 'x' -- 6x

assert :: Bool -> a -> a
assert False _ = error "*** assertion failed! ***"
assert _     x = x

test :: (Show a, Eq a) => String -> a -> a -> IO ()
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
          putStrLn ("g = " ++ show g)
          test "g + g" ((1 / 1) :+ (10 / 7)) (g Ring.+ g)
          test "g * g" (((-51) / 196) :+ (5 / 7)) (g Ring.* g)
--          putStrLn ("(n1 + n1) = " ++ show (Numeral.n1 Ring.+ Numeral.n1))
--          putStrLn ("(n2 * n3) = " ++ show (Numeral.n2 Prelude.* Numeral.n3))
          putStrLn ("expr = " ++ show expr)
          putStrLn ("derivative expr = " ++ show (derivative expr))
          test "ddx expr" dexpr (ddx expr)
          putStrLn ("eval (ddx expr) = " ++ show (evalExpr 'x' 5 (ddx expr)))


