{-
An evaluator for lambda expressions. The evaluator uses lexical scoping and removes all
symbolic (name-based) references in a compile-like first pass over the expression.

Translated from original Java sources here: https://github.com/pmilne/lambda/blob/master/src/lambda/Evaluator.java
-}
module Lambda.Lambda1 where

import           Data.List
import           Prelude   hiding (exp)

data Fun = Fun
  { name_     :: String
  , function_ :: Primitive -> Primitive
  }

instance Eq Fun where
  Fun n1 f1 == Fun n2 f2 = n1 == n2

instance Show Fun where
  show (Fun n f) = n

data Primitive
  = Int0 Int
  | Str0 String
  | Fun0 Fun
  deriving (Eq, Show)

data Expression
  = Constant !Primitive
  | Symbol !String
  | Lambda !Expression
           !Expression
  | Application !Expression
                !Expression
  deriving (Eq, Show)

varName :: Expression -> String
varName (Symbol s) = s
varName _          = error "variable wasn't a variable!!!"

toFunction :: Primitive -> Fun
toFunction (Fun0 f) = f
toFunction p        = undefined

toFunction2 :: Primitive -> (Primitive -> Primitive)
toFunction2 f = function_ (toFunction f)

toInt :: Primitive -> Int
toInt (Int0 i) = i
toInt p        = undefined

getOrFail :: Maybe a -> a
getOrFail (Just x) = x
getOrFail Nothing  = error "This didn't happen"

{-
{-# ANN module "HLint: ignore Use id" #-}
{-# ANN module "HLint: ignore Use Control.Exception.catch" #-}
{-# ANN module "HLint: ignore Defined but not used" #-}
-}

{-# ANN module "HLint: ignore Avoid lambda" #-}
{-# ANN module "HLint: ignore Use const" #-}
{-# ANN module "HLint: ignore Eta reduce" #-}

createCompiler :: [String] -> Expression -> [Primitive] -> Primitive
createCompiler nameStack exp0 = rec exp0
  where
    rec e =
      case e of
        Constant value -> \env -> value
        Symbol name ->
          let index = getOrFail (elemIndex name nameStack)
          in \env -> env !! index
        Application fun arg ->
          let fun0 = rec fun
          in let arg0 = rec arg
             in \env -> toFunction2 (fun0 env) (arg0 env)
        Lambda var exp ->
          let var0 = varName var
          in let body0 = createCompiler (var0 : nameStack) exp
             in \env -> Fun0 (Fun "hello" (\arg -> body0 (arg : env)))

eval :: Expression -> Primitive
eval input = createCompiler [] input []
