{-
From: http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
-}
module Expression where

import Prelude hiding ((+), (-), negate, (*), (/), (^), exp, log, sin , cos, tan, sqrt, asin, acos, atan)
import Field
import Exponentiative
import Trigonometric
import Debug.Trace

data Fn a = Fn {
  name_ :: String,
  value_ :: a -> a,
  inverse_ :: Expression a -> Expression a,
  derivative_:: Expression a -> Expression a
}

instance Eq (Fn a) where
  Fn name1 _ _ _ == Fn name2 _ _ _= name1 == name2

instance Show (Fn a) where
  show (Fn name1 _ _ _) = name1

data Expression a = Const a
                  | Var String
                  | Fun (Fn a)
                  | App (Expression a) (Expression a)
                  | Sum (Expression a) (Expression a)
                  | Neg (Expression a)
                  | Prd (Expression a) (Expression a)
                  | Div (Expression a) (Expression a)
                  | Pow (Expression a) (Expression a)
                  deriving (Eq)

instance (Show a) => Show (Expression a) where
 show (Const a) = show a
 show (Var a)   = a
 show (Fun f)   = name_ f
 show (App f a) = "(" ++ show f ++ " " ++ show a ++ ")"
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
  (Div n1 d1) * (Div n2 d2)
                    = Div (n1 * n2) (d1 * d2)
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
  ln (Const a)      = Const (ln a)
  ln a              = App (Fun (Fn "ln" ln exp (\x -> one / x))) a
  exp (Const a)     = Const (exp a)
  exp a             = App (Fun (Fn "exp" exp ln exp)) a
  sqrt              = App (Fun (Fn "sqrt" sqrt (^ two) (\x -> one / (two * sqrt x))))
  two               = Const two


-- https://en.wikipedia.org/wiki/Differentiation_of_trigonometric_functions
instance (Eq a, Field a, Exponentiative a, Trigonometric a) => Trigonometric (Expression a) where
  sin = App (Fun (Fn "sin" sin asin cos))
  cos = App (Fun (Fn "cos" cos acos (neg . sin)))
  tan = App (Fun (Fn "tan" tan atan (\x -> one / cos x^two)))
  asin = App (Fun (Fn "asin" asin sin (\x -> one / sqrt (one - x^two))))
  acos = App (Fun (Fn "acos" acos cos (\x -> neg one / sqrt (one - x^two))))
  atan = App (Fun (Fn "atan" atan tan (\x -> one / (one + x^two))))

ev :: (Show a) => Fn a -> Expression a -> Expression a
ev fun (Const x) = Const (value_ fun x)
ev fun e = App (Fun fun) e

evalExpr :: (Show a, Eq a, Field a, Exponentiative a) => String -> a -> Expression a -> a
evalExpr nm val =
  rec where
  rec e = case e of
                         Const a        -> a
                         Var a          -> if a == nm then val else undefined
                         App (Fun fn) a -> value_ fn (rec a)
                         Neg a          -> neg (rec a)
                         Sum a b        -> rec a + rec b
                         Prd a b        -> rec a * rec b
                         Div a b        -> rec a / rec b
                         Pow a b        -> rec a ^ rec b

derivative :: (Show a, Eq a, Field a, Exponentiative a) => Expression a -> Expression a
derivative (Const _)            = zero
derivative (Var _)              = one
derivative (App (Fun f) a)      = derivative a * derivative_ f a -- chain rule
derivative (Neg a)              = neg (derivative a)
derivative (Sum a b)            = derivative a + derivative b
derivative (Prd a b)            = a * derivative b + derivative a * b --product rule (ab' + a'b)
derivative (Div a b)            = (derivative a * b - a * derivative b) / b ^ two -- quotient rule ( (a'b - b'a) / b^2 )
derivative (Pow a (Const n))    = Const n * derivative a * a ^ Const (n - one) --specialised power rule (xa^(n-1) * a')
derivative (Pow f g)            = f ^ g * (derivative f * g / f + derivative g * ln f) --general power rule: https://en.wikipedia.org/wiki/Differentiation_rules#Generalized_power_rule

inverse :: (Show a, Eq a, Field a, Exponentiative a) => Expression a -> Expression a
inverse (Const _)            = undefined
inverse (Var x)              = Var x
inverse (App (Fun f) a)      = inverse_ f (inverse a)
inverse (Neg a)              = neg a
--inverse (Sum a b)            = derivative a + derivative b
--inverse (Prd a b)            = a * derivative b + b * derivative a --product rule (ab' + a'b)
--inverse (Div a b)            = (derivative a * b - a * derivative b) / b ^ two -- quotient rule ( (a'b - b'a) / b^2 )
--inverse (Pow a (Const n))    = Const n * derivative a * a ^ Const (n - one) --specialised power rule (xa^(n-1) * a')
--inverse (Pow f g)            = f ^ g * (derivative f * g / f + derivative g * ln f) --general power rule: https://en.wikipedia.org/wiki/Differentiation_rules#Generalized_power_rule

