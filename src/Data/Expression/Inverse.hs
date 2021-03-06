module Data.Expression.Inverse where

import           Prelude        (Eq, Show, error, otherwise, show, undefined, ($), (++), (==))

import           Domains.Applicable
import           Domains.Exponentiative
import           Domains.Field
import           Domains.Trigonometric

import           Data.Expression

{-# ANN module "HLint: ignore Redundant bracket" #-}

{-# ANN module "HLint: ignore Avoid lambda" #-}

inv2 :: (Eq a, Field a, Exponentiative a, Trigonometric a, Applicable a) => Fn a -> (Expression a -> Expression a)
inv2 f =
  case (name_ f) of
    "ln"   -> exp
    "exp"  -> ln
    "sqrt" -> \x -> x ^ two
    "sin"  -> asin
    "cos"  -> acos
    "tan"  -> atan
    "asin" -> sin
    "acos" -> cos
    "atan" -> tan

inverse :: (Show a, Eq a, Field a, Exponentiative a, Trigonometric a, Applicable a) => Expression a -> Expression a
inverse (Lambda var body) = rec body
  where
    rec e =
      case e of
        (Const _)         -> undefined
        (Var x)
          | e == var      -> var ~> var
          | otherwise     -> undefined
        (App f a)         -> var ~> apply (rec a) (apply (inverse f) var)
        (Sum (Const a) b) -> var ~> apply (rec b) (neg (Const a) + var)
        (Sum a (Const b)) -> var ~> apply (rec a) (var - Const b)
        (Sum _ _)         -> undefined
        (Neg a)           -> var ~> apply (rec a) (neg var)
        (Prd (Const a) b) -> var ~> apply (rec b) (reciprocal (Const a) * var)
        (Prd a (Const b)) -> var ~> apply (rec a) (var / Const b)
        (Prd _ _)         -> undefined
        (Rcp a)           -> var ~> apply (rec a) (reciprocal var)
        (Pow a (Const n)) -> var ~> apply (rec a) (var ^ reciprocal (Const n))
        (Pow (Const a) n) -> var ~> apply (rec n) (log (Const a) var)
        (Pow _ _)         -> undefined
inverse (Fun f) =
  let var = Var "x"
  in var ~> inv2 f var
inverse e = error $ "Error: inverse " ++ show e
