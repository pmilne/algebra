{-
An evaluator for lambda expressions. The evaluator uses lexical scoping and removes all
symbolic (name-based) references in a compile-like first pass over the expression.

Translated from original Java sources here: https://github.com/pmilne/lambda/blob/master/src/lambda/Evaluator.java
-}

module Lambda where

import Data.List

data Fun = Fun {name_ :: String, function_ :: Primitive -> Primitive}

instance Eq Fun where
  Fun n1 f1 == Fun n2 f2 = n1 == n2

instance Show Fun where
  show (Fun n f) = n

data Primitive = Int0 Int
               | Str0 String
               | Fun0 Fun
               deriving (Eq, Show)


data Expression = Constant !Primitive
                | Symbol String
                | Lambda !Expression !Expression
                | Application !Expression !Expression
                deriving (Eq, Show)

varName :: Expression -> String
varName (Symbol s) = s
varName _ = error "variable wasn't a variable!!!"

toFunction :: Primitive -> Fun
toFunction (Fun0 f) = f
toFunction p = undefined

toFunction2 :: Primitive -> (Primitive -> Primitive)
toFunction2 f = function_ (toFunction f)

toInt :: Primitive -> Int
toInt (Int0 i) = i
toInt p = undefined

getOrFail :: Maybe a -> a
getOrFail (Just x) = x
getOrFail Nothing = error "This didin't happen"

createCompiler :: [String] -> Expression -> [Primitive] -> Primitive
createCompiler nameStack exp env =
    rec exp where
    rec exp = case exp of
                    Constant value -> value

                    Symbol name -> env !! getOrFail (elemIndex name nameStack)

                    Application fun arg -> toFunction2 (rec fun) (rec arg)

                    Lambda var body ->
                        let var0 = varName var in
                        let body0 = createCompiler (var0 : nameStack) body in
                        Fun0 (Fun "hello" (\arg -> body0 (arg : env)))


eval :: Expression -> Primitive
eval input = createCompiler [] input []
