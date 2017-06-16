{-
An evaluator for lambda expressions. The evaluator uses lexical scoping and removes all
symbolic (name-based) references in a compile-like first pass over the expression.

Translated from original Java sources here: https://github.com/pmilne/lambda/blob/master/src/lambda/Evaluator.java
-}

module Lambda2 where

import Prelude hiding (exp)
import Data.List

data Primitive a = Val0 a
                 | Fun0 {name_ :: String, function_ :: Primitive a -> Primitive a}

instance (Eq a) => Eq (Primitive a) where
  Val0 v1   == Val0 v2   = v1 == v2
  Fun0 n1 _ == Fun0 n2 _ = n1 == n2
  _         == _         = False

instance (Show a) => Show (Primitive a) where
  show (Val0 v)   = show v
  show (Fun0 n _) = n

data Expression a = Constant !(Primitive a)
                  | Symbol String
                  | Lambda !(Expression a)!(Expression a)
                  | Application !(Expression a) !(Expression a)
                  deriving (Eq, Show)

varName :: Expression a -> String
varName (Symbol s) = s
varName _          = error "Formal parameter wasn't a symbol"

toValue :: Primitive a -> a
toValue (Val0 x) = x
toValue _        = undefined

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
                        \env -> function_ (fun0 env) (arg0 env)

                    Lambda var body ->
                        let var0 = varName var in
                        let exp0 = createCompiler (var0 : nameStack) body in
                        \env -> Fun0 "hello" (\arg -> exp0 (arg : env))


eval :: Expression a -> Primitive a
eval input = createCompiler [] input []
