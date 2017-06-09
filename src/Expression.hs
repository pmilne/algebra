{-
From: http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
-}
module Expression where

import Prelude hiding ((+), (-), negate, (*), (/), (^), exp, log)
import Field
import Exponentiative

data Expression a = Const a
                  | Var Char
                  | Sum (Expression a) (Expression a)
                  | Neg (Expression a)
                  | Prd (Expression a) (Expression a)
                  | Div (Expression a) (Expression a)
                  | Pow (Expression a) (Expression a)
                  | Log (Expression a)
                  deriving (Eq)

instance (Eq a, Field a, Exponentiative a) => Additive (Expression a) where
  Const a + Const b = Const (a + b)
  Const a + b       = if a == zero then b else Sum (Const a) b
  a       + Const b = if b == zero then a else Sum a (Const b)
  a + b             = Sum a b
  zero              = Const zero

instance (Eq a, Field a, Exponentiative a) => Multiplicative (Expression a) where
  a * b    = simplify (Prd a b)
  one      = Const one

instance (Eq a, Field a, Exponentiative a) => Negatable (Expression a) where
  neg a = simplify (Neg a)

instance (Eq a, Field a, Exponentiative a) => Subtractive (Expression a) where

instance (Eq a, Field a, Exponentiative a) => Ring (Expression a) where

instance (Eq a, Field a, Exponentiative a) => Invertable (Expression a) where
  inv a = simplify (Div one a)

instance (Eq a, Field a, Exponentiative a) => Field (Expression a) where
  a / b = simplify (Div a b)

instance (Eq a, Field a, Exponentiative a) => Exponentiative (Expression a) where
  a ^ b = simplify (Pow a b)
  log a = simplify (Log a)

instance (Show a) => Show (Expression a) where
 show (Var a)   = show a
 show (Const a) = show a
 show (Log a)   = "(log " ++ show a ++ ")"
 show (Sum a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
 show (Neg a)   = "(" ++ "-" ++ show a ++ ")"
 show (Prd a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
 show (Pow a b) = "(" ++ show a ++ " ^ " ++ show b ++ ")"
 show (Div a b) = "(" ++ show a ++ " / " ++ show b ++ ")"

simplify :: (Eq a, Field a, Exponentiative a) => Expression a -> Expression a
--additive identities
--multiplicative identities
simplify (Prd (Const a) (Const b)) = Const (a * b)
--put constants first in a product
simplify (Prd a b@(Const _)) | b == zero = zero | b == one = a | otherwise = Prd b a
--associativity
simplify (Prd (Const a) (Prd (Const b) expr)) = Prd (Const (a * b)) expr
-- and back
simplify (e@(Prd (Const a) b)) | a == zero = zero | a == one = b | otherwise = e
--power identities
simplify (Pow (Const a) (Const b)) = Const (a ^ b)
simplify (e@(Pow a (Const b))) | b == zero = one | b == one = a | otherwise = e
simplify (e@(Pow (Const a) _)) | a == one = one | otherwise = e

simplify (Div (Const a) (Const b)) = Const (a / b)
simplify (e@(Div a (Const b))) | b == zero = error "Divide by zero!" | b == one = a | otherwise = e
simplify e@(Div (Var a) (Var b)) = if a == b then one else e

simplify (Neg (Const a))  = Const (neg a)

simplify (Log (Const a))  = Const (log a)

simplify x          = x

mapExpr :: (Expression t -> Expression t) -> (Expression t -> Expression t)
mapExpr f exp =
  let walk e = case e of (Const a) -> f (Const a)
                         (Var a)   -> f (Var a)
                         (Neg a)   -> f (Neg (walk a))
                         (Log a)   -> f (Log (walk a))
                         (Sum a b) -> f (Sum (walk a) (walk b))
                         (Prd a b) -> f (Prd (walk a) (walk b))
                         (Div a b) -> f (Div (walk a) (walk b))
                         (Pow a b) -> f (Pow (walk a) (walk b))
  in walk exp

substitute :: Char -> a -> Expression a -> Expression a
substitute c val (Var x) = if x == c then Const val else Var x
substitute _ _ exp = exp

evalExpr :: (Eq a, Field a, Exponentiative a) => Char -> a -> Expression a -> Expression a
evalExpr c val = mapExpr (simplify . substitute c val)

derivative :: (Eq a, Field a, Exponentiative a) => Expression a -> Expression a
derivative (Const _)         = zero
derivative (Var _)           = one
derivative (Neg f)           = Neg (derivative f)
derivative (Sum a b)         = derivative a + derivative b
derivative (Prd a b)         = a * derivative b + b * derivative a --product rule (ab' + a'b)
derivative (Div a b)         = (derivative a * b - a * derivative b) / b ^ Const (one + one) -- quotient rule ( (a'b - b'a) / b^2 )
derivative (Pow a (Const x)) = Const x * derivative a * a ^ Const (x - one) --specialised power rule (xa^(x-1) * a')
derivative (Pow f g)         = f ^ g * (derivative f * g / f + derivative g * log f) --general power rule: https://en.wikipedia.org/wiki/Differentiation_rules#Generalized_power_rule
derivative (Log a)           = derivative a / a

