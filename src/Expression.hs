{-
From: http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
-}
module Expression where

import Prelude hiding ((+), (-), negate, (*), (/), (^), exp, log, sin , cos, tan, sqrt, asin, acos, atan)
import Field
import Exponentiative
import Trigonometric

data Fn a = Fn {
  name :: String,
  deriv:: a -> a
}

instance Eq (Fn a) where
  Fn name1 _ == Fn name2 _ = name1 == name2

instance Show (Fn a) where
  show (Fn name1 _) = name1

data Expression a = Const a
                  | Var String
                  | App (Fn (Expression a)) (Expression a)
                  | Sum (Expression a) (Expression a)
                  | Neg (Expression a)
                  | Prd (Expression a) (Expression a)
                  | Div (Expression a) (Expression a)
                  | Pow (Expression a) (Expression a)
                  | Log (Expression a)
                  deriving (Eq)

instance (Show a) => Show (Expression a) where
 show (Const a) = show a
 show (Var a)   = a
 show (App f a) = "(" ++ show f ++ " " ++ show a ++ ")"
 show (Log a)   = "(log " ++ show a ++ ")"
 show (Sum a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
 show (Neg a)   = "(" ++ "-" ++ show a ++ ")"
 show (Prd a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
 show (Pow a b) = "(" ++ show a ++ "^" ++ show b ++ ")"
 show (Div a b) = "(" ++ show a ++ "/" ++ show b ++ ")"

instance (Eq a, Additive a) => Additive (Expression a) where
  Const a + Const b = Const (a + b)
  Const a + b       = if a == zero then b else Sum (Const a) b
  a       + Const b = if b == zero then a else Sum a (Const b)
  a + b             = Sum a b
  zero              = Const zero

instance (Negatable a) => Negatable (Expression a) where
  neg (Const a) = Const (neg a)
  neg a = Neg a

instance (Eq a, Subtractive a) => Subtractive (Expression a) where

instance (Eq a, Additive a, Multiplicative a) => Multiplicative (Expression a) where
  Const a * Const b = Const (a * b)
  a       * Const b | b == zero = zero
                    | b == one = a
                    | otherwise = Prd (Const b) a -- if commutative we can move the constant to the front
  Const a * (Prd (Const b) c)  -- if associative, gather constants
                    = Prd (Const (a * b)) c
  Const a * b       | a == zero = zero
                    | a == one = b
                    | otherwise = Prd (Const a) b
  a       * b       = Prd a b
  one               = Const one

instance (Eq a, Ring a) => Ring (Expression a) where

instance (Eq a, Field a) => Invertable (Expression a) where
  inv = Div one

instance (Eq a, Field a) => Field (Expression a) where
  Const a / Const b = Const (a / b)
  a       / Const b | b == zero = error "Divide by zero!"
                    | b == one = a
                    | otherwise = Div a (Const b)
  Const a / b       | a == zero = zero
                    | otherwise = Div (Const a) b
  Div a b / c       = Div a (b * c)
  a       / Div b c = Div (a * c) b
  a       / b       = if a == b then one else Div a b

instance (Eq a, Field a, Exponentiative a) => Exponentiative (Expression a) where
  Const a ^ Const b = Const (a ^ b)
  a       ^ Const b | b == zero = one
                    | b == one = a
                    | otherwise = Pow a (Const b)
  Const a ^ b       | a == zero = zero
                    | a == one = one
                    | otherwise = Pow (Const a) b
  a       ^ b       = Pow a b
  log (Const a)     = Const (log a)
  log a             = Log a
  sqrt              = App (Fn "sqrt" (\x -> one / (two * sqrt x)))
  two               = Const two


-- https://en.wikipedia.org/wiki/Differentiation_of_trigonometric_functions
instance (Eq a, Field a, Exponentiative a, Trigonometric a) => Trigonometric (Expression a) where
  sin = App (Fn "sin" cos)
  cos = App (Fn "cos" (neg . sin))
  tan = App (Fn "tan" (\x -> one / cos x^two))
  asin = App (Fn "asin" (\x -> one / sqrt (one - x^two)))
  acos = App (Fn "acos" (\x -> neg one / sqrt (one - x^two)))
  atan = App (Fn "atan" (\x -> one / (one + x^two)))

mapExpr :: (Expression t -> Expression t) -> (Expression t -> Expression t)
mapExpr f exp =
  let walk e = case e of (Const a)      -> f (Const a)
                         (Var a)        -> f (Var a)
                         (App fun a)    -> f (App fun (walk a)) -- ?? should we transform the function here?
                         (Neg a)        -> f (Neg (walk a))
                         (Log a)        -> f (Log (walk a))
                         (Sum a b)      -> f (Sum (walk a) (walk b))
                         (Prd a b)      -> f (Prd (walk a) (walk b))
                         (Div a b)      -> f (Div (walk a) (walk b))
                         (Pow a b)      -> f (Pow (walk a) (walk b))
  in walk exp

substitute :: String -> a -> Expression a -> Expression a
substitute c val (Var x) = if x == c then Const val else Var x
substitute _ _ exp = exp

evalExpr :: (Eq a, Field a, Exponentiative a) => String -> a -> Expression a -> Expression a
evalExpr c val = mapExpr (substitute c val)

derivative :: (Eq a, Field a, Exponentiative a) => Expression a -> Expression a
derivative (Const _)            = zero
derivative (Var _)              = one
derivative (App f a)            = derivative a * deriv f a -- chain rule
derivative (Neg a)              = neg (derivative a)
derivative (Sum a b)            = derivative a + derivative b
derivative (Prd a b)            = a * derivative b + b * derivative a --product rule (ab' + a'b)
derivative (Div a b)            = (derivative a * b - a * derivative b) / b ^ two -- quotient rule ( (a'b - b'a) / b^2 )
derivative (Pow a (Const n))    = Const n * derivative a * a ^ Const (n - one) --specialised power rule (xa^(n-1) * a')
derivative (Pow f g)            = f ^ g * (derivative f * g / f + derivative g * log f) --general power rule: https://en.wikipedia.org/wiki/Differentiation_rules#Generalized_power_rule
derivative (Log a)              = derivative a / a

