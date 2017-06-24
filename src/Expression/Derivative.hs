module Expression.Derivative where

import           Prelude        (Eq, Show, error, otherwise, show, ($), (++), (==))

import           Applicable
import           Exponentiative
import           Field
import           Trigonometric

import           Expression

{-# ANN module "HLint: ignore Redundant bracket" #-}

{-# ANN module "HLint: ignore Avoid lambda" #-}

{-# ANN module "HLint: ignore Redundant lambda" #-}

deriv :: (Eq a, Field a, Exponentiative a, Trigonometric a) => Fn a -> (Expression a -> Expression a)
deriv f =
  case (name_ f) of
    "ln"   -> reciprocal
    "exp"  -> exp
    "sqrt" -> (\x -> one / (two * sqrt x))
    "sin"  -> cos
    "cos"  -> (\x -> neg (sin x))
    "tan"  -> (\x -> one / cos x ^ two)
    "asin" -> (\x -> (one / sqrt (one - x ^ two)))
    "acos" -> (\x -> (neg one / sqrt (one - x ^ two)))
    "atan" -> (\x -> (one / (one + x ^ two)))

derivative :: (Show a, Eq a, Field a, Exponentiative a, Trigonometric a, Applicable a) => Expression a -> Expression a
derivative (Lambda var body) = Lambda var (rec body)
  where
    rec e =
      case e of
        (Const _)         -> zero
        (Var _)
          | e == var      -> one
          | otherwise     -> zero
        (App f a)         -> rec a * (apply (derivative f) a) -- chain rule
        (Neg a)           -> neg (rec a)
        (Sum a b)         -> rec a + rec b
        (Prd a b)         -> a * rec b + rec a * b --product rule (ab' + a'b)
        (Inv a)           -> neg (rec a) / (a ^ two)
     -- (Div a b) -> (derivative a * b - a * derivative b) / b ^ two -- quotient rule ( (a'b - b'a) / b^2 )
        (Pow a (Const n)) -> Const n * rec a * a ^ Const (n - one) --specialised power rule (xa^(n-1) * a')
        (Pow f g)         -> f ^ g * (rec f * g / f + rec g * ln f) --general power rule: https://en.wikipedia.org/wiki/Differentiation_rules#Generalized_power_rule
derivative (Fun f) =
  let var = Var "x"
  in var ~> (deriv f var)
derivative e = error $ "Error: dd " ++ show e
