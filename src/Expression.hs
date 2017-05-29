{-
From: http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
-}
module Expression where

import Prelude hiding ((+), (-), negate, (*), (/), (^), exp)
import Ring
import Field
import Exponentiable

data Expression a = Var Char
                  | Const a
                  | Sum (Expression a) (Expression a)
                  | Neg (Expression a)
                  | Prd (Expression a) (Expression a)
                  | Pow (Expression a) (Expression a)
                  | Div (Expression a) (Expression a)
                  deriving (Eq)

instance (Ring a) => Ring (Expression a) where
  (+) = Sum
  (*) = Prd
  negate = Neg
  zero = Const zero
  one = Const one

instance (Field a) => Field (Expression a) where
  (/) = Div

instance (Exponentiable a) => Exponentiable (Expression a) where
  (^) = Pow

instance (Show a) => Show (Expression a) where
 show (Var a) = show a
 show (Const a) = show a
 show (Sum a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
 show (Neg a) = "(" ++ "-" ++ show a ++ ")"
 show (Prd a b) = "(" ++ show a ++ " * " ++ show b ++ ")"
 show (Pow a b) = "(" ++ show a ++ " ^ " ++ show b ++ ")"
 show (Div a b) = "(" ++ show a ++ " / " ++ show b ++ ")"

simplify :: (Eq a, Field a, Exponentiable a) => Expression a -> Expression a
--additive identities
simplify (Sum (Const a) (Const b)) = Const (a + b)
simplify (e@(Sum (Const a) b)) = if a == zero then b else e
simplify (e@(Sum a (Const b))) = if b == zero then a else e
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

simplify (Neg (Const a))  = Const (negate a)

simplify x          = x

mapExpr :: (Expression t -> Expression t) -> (Expression t -> Expression t)
mapExpr f exp =
  let walk e = case e of
        (Const a) -> f (Const a)
        (Var a)   -> f (Var a)
        (Neg a)   -> f (Neg (walk a))
        (Sum a b) -> f (Sum (walk a) (walk b))
        (Prd a b) -> f (Prd (walk a) (walk b))
        (Div a b) -> f (Div (walk a) (walk b))
        (Pow a b) -> f (Pow (walk a) (walk b))
   in walk exp

fullSimplify :: (Eq t, Field t, Exponentiable t) => Expression t -> Expression t
fullSimplify = mapExpr simplify

substitute :: Char -> a -> Expression a -> Expression a
substitute c val (Var x) = if x == c then Const val else Var x
substitute _ _ exp = exp

evalExpr :: (Eq a, Field a, Exponentiable a) => Char -> a -> Expression a -> Expression a
evalExpr c val exp = fullSimplify (mapExpr (substitute c val) exp)

derivative :: (Field a) => Expression a -> Expression a
derivative (Const _)         = zero
derivative (Var _)           = one
derivative (Neg f)           = Neg (derivative f)
derivative (Sum a b)         = derivative a + derivative b
derivative (Prd a b)         = a * derivative b + b * derivative a --product rule (ab' + a'b)
derivative (Div a b)         = (derivative a * b - a * derivative b) / Pow b (Const (one + one)) -- quotient rule ( (a'b - b'a) / b^2 )
derivative (Pow a (Const x)) = Const x * derivative a * Pow a (Const (x - one)) --specialised power rule (xa^(x-1) * a')
derivative (Pow _ _)         = undefined --general power rule: https://en.wikipedia.org/wiki/Differentiation_rules#Generalized_power_rule

ddx :: (Eq a, Field a, Exponentiable a) => Expression a -> Expression a
ddx = fullSimplify . derivative

{-
ddxs :: (Ring a, Exponentiable a, Field a, Eq a) => Expr a -> [Expr a]
ddxs = iterate ddx

nthDerivative :: (Ring a, Exponentiable a, Field a, Eq a) => Int -> Expr a -> Expr a
nthDerivative n = foldr1 (.) (replicate n ddx)
-}
