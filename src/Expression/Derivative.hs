module Expression.Derivative where

import           Applicable
import           Exponentiative
import           Expression
import           Field
import           Prelude        hiding (acos, asin, atan, cos, exp, log, negate, sin, sqrt, tan, (*), (+), (-), (/),
                                 (^))
import           Trigonometric

{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}

data DD a =
  DD !(Expression a)

getExp :: DD a -> Expression a
getExp (DD a) = a

instance (Eq a, Negatable a) => Negatable (DD a) where
  neg (DD x) = DD (neg x)

instance (Eq a, Field a, Exponentiative a, Trigonometric a) => Trigonometric (DD a) where
  sin (DD x) = DD (cos x)
  cos (DD x) = DD (neg (sin x))
  tan (DD x) = DD (one / cos x ^ two)
  asin (DD x) = DD (one / sqrt (one - x ^ two))
  acos (DD x) = DD (neg one / sqrt (one - x ^ two))
  atan (DD x) = DD (one / (one + x ^ two))

deriv :: (Eq a, Field a, Exponentiative a, Trigonometric a, Applicable a) => Fn a -> (Expression a -> Expression a)
deriv f =
  case (name_ f) of
    "ln"   -> (\x -> one / x)
    "exp"  -> exp
    "sin"  -> cos
    "cos"  -> (\x -> neg (sin x))
    "tan"  -> (\x -> one / cos x ^ two)
    "asin" -> (\x -> (one / sqrt (one - x ^ two)))
    "acos" -> (\x -> (neg one / sqrt (one - x ^ two)))
    "atan" -> (\x -> (one / (one + x ^ two)))

derivative :: (Show a, Eq a, Field a, Exponentiative a, Trigonometric a, Applicable a) => Expression a -> Expression a -- todo shouldn't have applicable here
derivative (Lambda var body) = Lambda var (rec body)
  where
    rec e =
      case e of
        (Const _) -> zero
        (Var _) ->
          if e == var
            then one
            else zero
        (App f a) -> rec a * (apply (derivative f) a) -- chain rule
        (Neg a) -> neg (rec a)
        (Sum a b) -> rec a + rec b
        (Prd a b) -> a * rec b + rec a * b --product rule (ab' + a'b)
        (Inv a) -> neg (rec a) / (a ^ two)
                                  --derivative (Div a b)            -> (derivative a * b - a * derivative b) / b ^ two -- quotient rule ( (a'b - b'a) / b^2 )
        (Pow a (Const n)) -> Const n * rec a * a ^ Const (n - one) --specialised power rule (xa^(n-1) * a')
        (Pow f g) -> f ^ g * (rec f * g / f + rec g * ln f) --general power rule: https://en.wikipedia.org/wiki/Differentiation_rules#Generalized_power_rule
derivative (Fun f) =
  let var = Var "d"
  in var ~> (deriv f var)
derivative e = error $ "Error: dd " ++ show e
