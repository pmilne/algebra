{-
An evaluator for lambda expressions. The evaluator uses lexical scoping and removes all
symbolic (name-based) references in a compile-like first pass over the expression.

Translated from original Java sources here: https://github.com/pmilne/lambda/blob/master/src/lambda/Evaluator.java
-}

module Lambda2 where

import Prelude hiding (exp)
import Data.List

data Primitive a = Value {value_ :: a}
                 | Function {function_ :: Primitive a -> Primitive a}

instance (Show a) => Show (Primitive a) where
  show (Value v) = show v
  show (Function _) = "<function>"

data Expression a = Constant !(Primitive a)
                  | Symbol !String
                  | Lambda !String !(Expression a)
                  | Application !(Expression a) !(Expression a)
                  deriving (Show)

getOrFail :: Maybe a -> a
getOrFail (Just x) = x
getOrFail Nothing = error "This didn't happen. "

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
                        let body0 = createCompiler (var : nameStack) body in
                        \env -> Function (\arg -> body0 (arg : env))

eval :: Expression a -> Primitive a
eval input = createCompiler [] input []
