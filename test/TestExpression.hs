module TestExpression where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd, Rational)
import Ring
import Euclidean
import Rational
import Exponentiative

import Expression

import TestUtil

instance Exponentiative (Rational a) where
  a ^ b = undefined
  log a = undefined

var :: Expression (Rational Integer)
var = Var "x"

pc :: (Euclidean a, Ring a) => a -> Expression (Rational a)
pc x = Const (rational1 x)

xp1 :: Expression (Rational Integer)
xp1 = var + pc 1

e0, e1, e2 :: Expression (Rational Integer)
--e0 = xp1 * xp1 -- (x + 1)^2
e0 = xp1 -- (x + 1)
e1 = pc 3 * Pow var (pc 2) -- 3x^2
e2 = Pow var var -- x ^ x

d0, d1, d2 :: Expression (Rational Integer)
d0 = pc 1
d1 = pc 6 * var -- 6x
d2 = Pow var var * (pc 1 + Log var)

run :: IO ()
run = do
          test ("derivative " ++ show e0) d0 (derivative e0)
          test ("derivative " ++ show e1) d1 (derivative e1)
          test ("derivative " ++ show e2) d2 (derivative e2)

          putStrLn ("eval (ddx expr) = " ++ show (evalExpr "x" (rational1 5) (derivative e2)))
          putStrLn ("expr = " ++ show e2)
