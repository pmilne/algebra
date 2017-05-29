{-
From: http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
-}
module Expr where

import Prelude hiding ((+), (-), negate, (*), (/), (^), exp)
import Ring
import Field
import Exponentiable

infixl 5 :*:

data Expr a = Var Char
            | Const a
            | Sum (Expr a) (Expr a)
            | Negate (Expr a)
            | (Expr a) :*: (Expr a)
            | Pow (Expr a) (Expr a)
            | Div (Expr a) (Expr a)
            deriving (Eq)

instance (Ring a) => Ring (Expr a) where
  (+) = Sum
  (*) = (:*:)
  negate = Negate
  zero = Const zero
  one = Const one

instance (Field a) => Field (Expr a) where
  (/) = Div

instance (Exponentiable a) => Exponentiable (Expr a) where
  (^) = Pow

instance (Show a) => Show (Expr a) where
 show (Var a) = show a
 show (Const a) = show a
 show (Sum a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
 show (Negate a) = "(" ++ "-" ++ show a ++ ")"
 show (a :*: b) = "(" ++ show a ++ " * " ++ show b ++ ")"
 show (Pow a b) = "(" ++ show a ++ " ^ " ++ show b ++ ")"
 show (Div a b) = "(" ++ show a ++ " / " ++ show b ++ ")"

simplify :: (Eq a, Field a, Exponentiable a) => Expr a -> Expr a
--additive identities
simplify (Sum (Const a) (Const b)) = Const (a + b)
simplify (e@(Sum (Const a) b)) = if a == zero then b else e
simplify (e@(Sum a (Const b))) = if b == zero then a else e
--multiplicative identities
simplify (Const a :*: Const b) = Const (a * b)
--associativity identities
simplify (Const a :*: (Const b :*: expr)) = Const (a * b) :*: expr
simplify (Const a :*: expr :*: Const b)   = Const (a * b) :*: expr
simplify (expr :*: Const a :*: Const b)   = Const (a * b) :*: expr
-- and back
simplify (e@(Const a :*: b)) | a == zero = zero | a == one = b | otherwise = e
simplify (e@(a :*: Const b)) | b == zero = zero | b == one = a | otherwise = e
--power identities
simplify (Pow (Const a) (Const b)) = Const (a ^ b)
simplify (e@(Pow a (Const b))) | b == zero = one | b == one = a | otherwise = e
simplify (e@(Pow (Const a) _)) | a == one = one | otherwise = e

simplify (Div (Const a) (Const b)) = Const (a / b)
simplify (e@(Div a (Const b))) | b == zero = error "Divide by zero!" | b == one = a | otherwise = e

simplify (Negate (Const a))  = Const (negate a)

simplify x          = x

mapExpr :: (Expr t -> Expr t) -> (Expr t -> Expr t)
mapExpr f (Var a)  = f (Var a)
mapExpr f (Const a)  = f (Const a)
mapExpr f (Negate a)  = f (Negate (mapExpr f a))
mapExpr f (Sum a b)  = f (Sum (mapExpr f a) (mapExpr f b))
mapExpr f (a :*: b)  = f (mapExpr f a :*: mapExpr f b)
mapExpr f (Div a b)  = f (Div (mapExpr f a) (mapExpr f b))
mapExpr f (Pow a b)  = f (Pow (mapExpr f a) (mapExpr f b))

fullSimplify :: (Eq t, Field t, Exponentiable t) => Expr t -> Expr t
fullSimplify = mapExpr simplify

substitute :: Char -> a -> Expr a -> Expr a
substitute c val (Var x) = if x == c then Const val else Var x
substitute _ _ exp = exp

evalExpr :: (Eq a, Field a, Exponentiable a) => Char -> a -> Expr a -> Expr a
evalExpr c val exp = fullSimplify (mapExpr (substitute c val) exp)

derivative :: (Field a) => Expr a -> Expr a
derivative (Const _)       = zero
derivative (Var _)         = one
derivative (Negate f)      = Negate (derivative f)
derivative (Sum a b)       = derivative a + derivative b
derivative (a :*: b)       = a * derivative b + b * derivative a --product rule (ab' + a'b)
derivative (Div a b)       = (derivative a * b - a * derivative b) / Pow b (Const (one + one)) -- quotient rule ( (a'b - b'a) / b^2 )
derivative (Pow a (Const x)) = Const x * derivative a * Pow a (Const (x - one)) --power rule (xa^(x-1) * a')
derivative (Pow _ _) = undefined --requires general power rule: https://en.wikipedia.org/wiki/Differentiation_rules#Generalized_power_rule

ddx :: (Eq a, Field a, Exponentiable a) => Expr a -> Expr a
ddx = fullSimplify . derivative

{-
ddxs :: (Ring a, Exponentiable a, Field a, Eq a) => Expr a -> [Expr a]
ddxs = iterate ddx

nthDerivative :: (Ring a, Exponentiable a, Field a, Eq a) => Int -> Expr a -> Expr a
nthDerivative n = foldr1 (.) (replicate n ddx)
-}
