{-
An evaluator for lambda expressions. The evaluator uses lexical scoping and removes all
symbolic (name-based) references in a compile-like first pass over the expression.

Translated from original Java sources here: https://github.com/pmilne/lambda/blob/master/src/lambda/Evaluator.java
-}

module Lambda2 where

import Prelude hiding (exp)
import Data.List

data (Fun a) = Fun {name_ :: String, function_ :: Primitive a -> Primitive a}

instance Eq (Fun a) where
  Fun n1 _ == Fun n2 _ = n1 == n2

instance Show (Fun a) where
  show (Fun n _) = n

data (Primitive a) = Int0 Int
               | Str0 String
               | Fun0 (Fun a)
               deriving (Eq, Show)


data Expression a = Constant !(Primitive a)
                | Symbol String
                | Lambda !(Expression a)!(Expression a)
                | Application !(Expression a) !(Expression a)
                deriving (Eq, Show)

varName :: Expression a -> String
varName (Symbol s) = s
varName _ = error "variable wasn't a variable!!!"

toFunction :: Primitive a -> Fun a
toFunction (Fun0 f) = f
toFunction _ = undefined

toFunction2 :: Primitive a -> Primitive a -> Primitive a
toFunction2 f = function_ (toFunction f)

toInt :: Primitive a -> Int
toInt (Int0 i) = i
toInt _ = undefined

getOrFail :: Maybe a -> a
getOrFail (Just x) = x
getOrFail Nothing = error "This didin't happen"

createCompiler :: [String] -> Expression a -> [Primitive a] -> Primitive a
createCompiler nameStack {-exp-} =
    rec {-exp-} where
    rec exp = case exp of
                    Constant value ->
                        \env -> value

                    Symbol name ->
                        let index = getOrFail (elemIndex name nameStack) in
                        \env -> env !! index

                    Application fun arg ->
                        let fun0 = rec fun in
                        let arg0 = rec arg in
                        \env -> toFunction2 (fun0 env) (arg0 env)

                    Lambda var body ->
                        let var0 = varName var in
                        let exp0 = createCompiler (var0 : nameStack) body in
                        \env -> Fun0 (Fun "hello" (\arg -> exp0 (arg : env)))


eval :: Expression a -> Primitive a
eval input = createCompiler [] input []
