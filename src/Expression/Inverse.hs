module Expression.Inverse where

import           Applicable
import           Exponentiative
import           Expression
import           Field
import           Prelude        hiding (acos, asin, atan, cos, exp, log, negate, sin, sqrt, tan, (*), (+), (-), (/),
                                 (^))
import           Trigonometric

{-# ANN module "HLint: ignore Redundant bracket" #-}
{-# ANN module "HLint: ignore Avoid lambda" #-}


inverse :: (Show a, Eq a, Field a, Exponentiative a, Applicable a) => Expression a -> Expression a
inverse (Lambda var body) = rec body
  where
    rec e =
      case e of
        (Const _)         -> undefined
        (Var x)           -> var ~> var
        (App f a)         -> var ~> apply (rec a) (apply (inverse f) var)
        (Sum (Const a) b) -> var ~> apply (rec b) (neg (Const a) + var)
        (Sum a (Const b)) -> var ~> apply (rec a) (var - Const b)
        (Sum _ _)         -> undefined
        (Neg a)           -> var ~> apply (rec a) (neg var)
        (Prd (Const a) b) -> var ~> apply (rec b) (inv (Const a) * var)
        (Prd a (Const b)) -> var ~> apply (rec a) (var / Const b)
        (Prd _ _)         -> undefined
        (Inv a)           -> var ~> apply (rec a) (inv var)
        (Pow a (Const n)) -> var ~> apply (rec a) (var ^ inv (Const n))
        (Pow (Const a) n) -> var ~> apply (rec n) (log (Const a) var)
        (Pow _ _)         -> undefined
inverse (Fun f) =
  let var = Var "x"
  in var ~> inverse_ f var
inverse e = error $ "Error: inverse " ++ show e
