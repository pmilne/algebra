module TestExpression where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd, Rational)
import Ring
import Field

import Expression

import TestUtil

var :: Expression Double
var = Var 'x'

xp1 = (var + Const 1)

e0, e1, e2 :: Expression Double
--e0 = xp1 * xp1 -- (x + 1)^2
e0 = xp1 -- (x + 1)
e1 = Const 3 * Pow var (Const 2) -- 3x^2
e2 = Pow var var -- x ^ x

d0, d1, d2 :: Expression Double
d0 = Const 1
d1 = Const 6 * var -- 6x
d2 = Pow var var * (Expression.Const 1.0 + Log var)

run :: IO ()
run = do
          test ("derivative " ++ show e0) d0 (derivative e0)
          test ("derivative " ++ show e1) d1 (derivative e1)
          test ("derivative " ++ show e2) d2 (derivative e2)

          putStrLn ("eval (ddx expr) = " ++ show (evalExpr 'x' 5 (derivative e2)))
          putStrLn ("expr = " ++ show e2)
