{-
From: http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
-}
module PowerableExpresion where

import Prelude hiding ((+), (-), negate, (*), (/), (^), exp, log)
import Ring
import Field
import Powerable

data PowerableExpresion a = Const a
                          | Pow (PowerableExpresion a) (PowerableExpresion a)
                          | Log (PowerableExpresion a)
                          deriving (Eq)

instance (Ring a) => Ring (PowerableExpresion a) where
  Const a + Const b = Const (a + b)
  Const a * Const b = Const (a * b)
  negate (Const a) = Const (negate a)
  zero = Const zero
  one = Const one

instance (Field a) => Field (PowerableExpresion a) where
  inv (Const a) = Const (inv a)

instance (Powerable a) => Powerable (PowerableExpresion a) where
  (^) = Pow
  log = Log

instance (Show a) => Show (PowerableExpresion a) where
 show (Const a) = show a
 show (Log a)   = "(log " ++ show a ++ ")"
 show (Pow a b) = "(" ++ show a ++ " ^ " ++ show b ++ ")"

simplify :: (Eq a, Field a, Powerable a) => PowerableExpresion a -> PowerableExpresion a
simplify (Pow (Const a) (Const b)) = Const (a ^ b)
simplify (e@(Pow a (Const b))) | b == zero = one | b == one = a | otherwise = e
simplify (e@(Pow (Const a) _)) | a == one = one | otherwise = e
simplify (Log (Const a))  = Const (log a)
simplify x          = x

mapExpr :: (PowerableExpresion t -> PowerableExpresion t) -> (PowerableExpresion t -> PowerableExpresion t)
mapExpr f exp =
  let walk e = case e of (Const a) -> f (Const a)
                         (Log a)   -> f (Log (walk a))
                         (Pow a b) -> f (Pow (walk a) (walk b))
  in walk exp

substitute :: Char -> a -> PowerableExpresion a -> PowerableExpresion a
--substitute c val (Var x) = if x == c then Const val else Var x
substitute _ _ exp = exp

evalExpr :: (Eq a, Field a, Powerable a) => Char -> a -> PowerableExpresion a -> PowerableExpresion a
evalExpr c val = mapExpr (simplify . substitute c val)

derivative :: (Field a, Powerable a) => PowerableExpresion a -> PowerableExpresion a
derivative (Const _)         = zero
derivative (Pow a (Const x)) = Const x * derivative a * a ^ Const (x - one) --specialised power rule (xa^(x-1) * a')
derivative (Pow f g)         = f ^ g * (derivative f * g / f + derivative g * log f) --general power rule: https://en.wikipedia.org/wiki/Differentiation_rules#Generalized_power_rule
derivative (Log a)           = derivative a / a
