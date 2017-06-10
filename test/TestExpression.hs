module TestExpression where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd, Rational, sin, cos, tan)

import Field
import Exponentiative
import Trigonometric

import Rational
import Expression

import TestUtil

instance Exponentiative (Rational a) where
  a ^ b = undefined
  log a = undefined

x :: Expression (Rational Integer)
x = Var "x"

pc :: (Euclidean a, Ring a) => a -> Expression (Rational a)
pc x = Const (rational1 x)

xp1 :: Expression (Rational Integer)
xp1 = x + pc 1

e0, e1, e2, e3 :: Expression (Rational Integer)
--e0 = xp1 * xp1 -- (x + 1)^2
e0 = xp1 -- (x + 1)
e1 = pc 3 * Pow x (pc 2) -- 3x^2
e2 = Pow x x -- x ^ x
e3 = Log (Log x)

d0, d1, d2, d3 :: Expression (Rational Integer)
d0 = pc 1
d1 = pc 6 * x -- 6x
d2 = Pow x x * (pc 1 + Log x)
d3 = pc 1 / (x * Log x)

x1 :: Expression Double
x1 = Var "x"

run :: IO ()
run = do
          test ("derivative " ++ show e0) d0 (derivative e0)
          test ("derivative " ++ show e1) d1 (derivative e1)
          test ("derivative " ++ show e2) d2 (derivative e2)
          test ("derivative " ++ show e3) d3 (derivative e3)
          putStrLn ("expr = " ++ show (derivative (sin x1)))
          putStrLn ("expr = " ++ show (derivative (x1 + sin x1)))
          putStrLn ("expr = " ++ show (derivative (sin (sin x1))))
          putStrLn ("expr = " ++ show (derivative (tan (tan x1))))
          putStrLn ("expr = " ++ show (derivative (derivative (x1 + sin x1))))

          putStrLn ("eval (ddx expr) = " ++ show (evalExpr "x" (rational1 5) (derivative e2)))
          putStrLn ("expr = " ++ show e2)
