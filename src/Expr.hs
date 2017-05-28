{-
From: http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
-}
module Expr where

import Prelude hiding ((+), (-), negate, (*), (^))
import Ring

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
  (^) = (:^:)
  negate = Negate
  zero = Const zero
  one = Const one

instance (Show a) => Show (Expr a) where
 show (Var a) = show a
 show (Const a) = show a
 show (a :+: b) = "(" ++ (show a) ++ " + " ++ (show b) ++ ")"
 show (Negate a) = "(" ++ "-" ++ show a ++ ")"
 show (a :*: b) = "(" ++ (show a) ++ " * " ++ (show b) ++ ")"
 show (a :^: b) = "(" ++ (show a) ++ " ^ " ++ (show b) ++ ")"
 show (a :/: b) = "(" ++ (show a) ++ " / " ++ (show b) ++ ")"

simplify :: (Eq a, Ring a) => Expr a -> Expr a
--additive identities
simplify (Const a :+: Const b) = Const (a + b)
simplify (a       :+: Const zero) = a
simplify (Const zero :+: a      ) = a
--multiplicative identities
simplify (Const a :*: Const b) = Const (a * b)
simplify (a       :*: Const zero) = Const zero
simplify (Const zero :*: a)       = Const zero
simplify (a       :*: Const one) = a
simplify (Const one :*: a)       = a
--power identities
simplify (a :^: Const zero)       = Const one
simplify (a :^: Const one)       = a
simplify (Const a :^: Const b) = Const (a ^ b)
simplify ((c :^: Const b) :^: Const a) = c :^: Const (a*b)

simplify (Const a :*: (Const b :*: expr)) = Const (a * b) :*: expr
simplify (Const a :*: expr :*: Const b)   = Const (a * b) :*: expr
simplify (expr :*: Const a :*: Const b)   = Const (a * b) :*: expr

simplify (Const zero :/: a        ) = Const zero
simplify (Const a :/: Const zero)   = error "Division by zero!"
simplify (a       :/: Const one)   = a
simplify (Const a :/: Const b)   | a == b = Const one -- only when a == b

simplify (Negate (Const a))  = Const (negate a)

simplify x          = id x

mapExpr :: ((Expr t) -> (Expr t)) -> (Expr t) -> (Expr t)
mapExpr f (Var a)  = f (Var a)
mapExpr f (Const a)  = f (Const a)
mapExpr f (Negate a)  = f (Negate (mapExpr f a))
mapExpr f (a :+: b)  = f ((mapExpr f a) :+: (mapExpr f b))
mapExpr f (a :*: b)  = f ((mapExpr f a) :*: (mapExpr f b))
mapExpr f (a :/: b)  = f ((mapExpr f a) :/: (mapExpr f b))
mapExpr f (a :^: b)  = f ((mapExpr f a) :^: (mapExpr f b))

fullSimplify :: (Ring t, Eq t) => Expr t -> Expr t
fullSimplify expr = mapExpr simplify expr

substitute :: Char -> a -> Expr a -> Expr a
substitute c val (Var x) = if x == c then Const val else Var x
substitute c val exp = exp

evalExpr :: (Ring a, Eq a) => Char -> a -> Expr a -> Expr a
evalExpr c val exp = fullSimplify (mapExpr (\e -> (substitute c val e)) exp)

derivative :: (Ring a) => Expr a -> Expr a
derivative (Var c)           = Const one
derivative (Const x)         = Const zero
derivative (a :+: b)         = (derivative a) :+: (derivative b)
--product rule (ab' + a'b)
derivative (a :*: b)         = (a :*: (derivative b)) :+: (b :*: (derivative a)) -- product rule
 --power rule (xa^(x-1) * a')
derivative (a :^: (Const x)) = ((Const x) :*: (a :^: (Const $ x - one))) :*: (derivative a)
 -- quotient rule ( (a'b - b'a) / b^2 )
derivative (a :/: b)         = ((derivative a :*: b) :+: (Negate (derivative b :*: a))) :/: (b :^: (Const (one + one)))
derivative expr              = error "I'm not a part of your system!" -- unsupported operation


ddx :: (Ring a, Eq a) => Expr a -> Expr a
ddx = fullSimplify . derivative

ddxs :: (Ring a, Eq a) => Expr a -> [Expr a]
ddxs = iterate ddx

nthDerivative :: (Ring a, Eq a) => Int -> Expr a -> Expr a
nthDerivative n = foldr1 (.) (replicate n ddx)