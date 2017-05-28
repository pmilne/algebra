{-
From: http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
-}
module Expr where

import Prelude hiding ((+), (-), negate, (*), (/), (^), exp)
import Ring
import Field
import Exponentiable

infixl 4 :+:
infixl 5 :*:, :/:
infixr 6 :^:

data Expr a = Var Char
            | Const a
            | (Expr a) :+: (Expr a)
            | Negate (Expr a)
            | (Expr a) :*: (Expr a)
            | (Expr a) :^: (Expr a)
            | (Expr a) :/: (Expr a)
            deriving (Eq)

instance (Ring a) => Ring (Expr a) where
  (+) = (:+:)
  (*) = (:*:)
  negate = Negate
  zero = Const zero
  one = Const one

instance (Field a) => Field (Expr a) where
  (/) = (:/:)

instance (Exponentiable a) => Exponentiable (Expr a) where
  (^) = (:^:)

instance (Show a) => Show (Expr a) where
 show (Var a) = show a
 show (Const a) = show a
 show (a :+: b) = "(" ++ (show a) ++ " + " ++ (show b) ++ ")"
 show (Negate a) = "(" ++ "-" ++ show a ++ ")"
 show (a :*: b) = "(" ++ (show a) ++ " * " ++ (show b) ++ ")"
 show (a :^: b) = "(" ++ (show a) ++ " ^ " ++ (show b) ++ ")"
 show (a :/: b) = "(" ++ (show a) ++ " / " ++ (show b) ++ ")"

simplify :: (Eq a, Exponentiable a, Field a, Field a, Ring a) => Expr a -> Expr a
--additive identities
simplify (Const a :+: Const b) = Const (a + b)
simplify (e@(Const a :+: b)) = if a == zero then b else e
simplify (e@(a :+: Const b)) = if b == zero then a else e
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
simplify (Const a :^: Const b) = Const (a ^ b)
simplify (e@(a :^: Const b)) | b == zero = one | b == one = a | otherwise = e
simplify (e@(Const a :^: b)) | a == one = one | otherwise = e

simplify (Const a :/: Const b) = Const (a / b)
simplify (e@(a :/: Const b)) | b == zero = error "Divide by zero!" | b == one = a | otherwise = e

simplify (Negate (Const a))  = Const (negate a)

simplify x          = x

mapExpr :: ((Expr t) -> (Expr t)) -> (Expr t) -> (Expr t)
mapExpr f (Var a)  = f (Var a)
mapExpr f (Const a)  = f (Const a)
mapExpr f (Negate a)  = f (Negate (mapExpr f a))
mapExpr f (a :+: b)  = f ((mapExpr f a) :+: (mapExpr f b))
mapExpr f (a :*: b)  = f ((mapExpr f a) :*: (mapExpr f b))
mapExpr f (a :/: b)  = f ((mapExpr f a) :/: (mapExpr f b))
mapExpr f (a :^: b)  = f ((mapExpr f a) :^: (mapExpr f b))

fullSimplify :: (Ring t, Exponentiable t, Field t, Eq t) => Expr t -> Expr t
fullSimplify = mapExpr simplify

substitute :: Char -> a -> Expr a -> Expr a
substitute c val (Var x) = if x == c then Const val else Var x
substitute c val exp = exp

evalExpr :: (Ring a, Exponentiable a, Field a, Eq a) => Char -> a -> Expr a -> Expr a
evalExpr c val exp = fullSimplify (mapExpr (\e -> (substitute c val e)) exp)

derivative :: (Ring a) => Expr a -> Expr a
derivative (Const c)         = Const zero
derivative (Var x)           = Const one
derivative (a :+: b)         = derivative a :+: derivative b
--product rule (ab' + a'b)
derivative (a :*: b)         = a * derivative b + b * derivative a -- product rule
 --power rule (xa^(x-1) * a')
derivative (a :^: (Const x)) = ((Const x) * (a :^: (Const $ x - one))) * (derivative a)
 -- quotient rule ( (a'b - b'a) / b^2 )
derivative (a :/: b)         = ((derivative a * b) + (Negate (derivative b * a))) :/: (b :^: (Const (one + one)))
derivative expr              = error "I'm not a part of your system!" -- unsupported operation


ddx :: (Ring a, Exponentiable a, Field a, Eq a) => Expr a -> Expr a
ddx = fullSimplify . derivative

{-
ddxs :: (Ring a, Exponentiable a, Field a, Eq a) => Expr a -> [Expr a]
ddxs = iterate ddx

nthDerivative :: (Ring a, Exponentiable a, Field a, Eq a) => Int -> Expr a -> Expr a
nthDerivative n = foldr1 (.) (replicate n ddx)
-}
