module TestExpression where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd, Rational)
import Ring
import Field

import Expression

import TestUtil

var :: Expression Double
var = Var 'x'

expr :: Expression Double
--expr = Const 3 * Pow var (Const 2) -- 3x^2
expr = Pow var var -- x ^ x

dexpr :: Expression Double
--dexpr = Const 6 * var -- 6x
dexpr = Pow var var * (Expression.Const 1.0 + Log var)

run :: IO ()
run = do
          putStrLn ("expr = " ++ show expr)
          test "derivative expr" dexpr (derivative expr)
          putStrLn ("eval (ddx expr) = " ++ show (evalExpr 'x' 5 (derivative expr)))
          putStrLn ("expr = " ++ show expr)
