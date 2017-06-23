module Expression.Eval where

import Expression
import Data.List

varName :: Expression a -> String
varName (Var s) = s
varName _ = error "Formal parameter to Lambda wasn't a symbol!"

getOrFail :: Maybe a -> a
getOrFail (Just x) = x
getOrFail Nothing = error "This didn't happen. "

-- Primitives, these will appear on the 'evaluation' stack

data Primitive a = Value    {value_    :: a}
                 | Function {function_ :: Primitive a -> Primitive a}

instance (Show a) => Show (Primitive a) where
  show (Value v) = show v
  show (Function _) = "<function>"

-- Evaluator / compiler

createCompiler :: [String] -> Expression a -> [Primitive a] -> Primitive a
createCompiler nameStack {-exp-} =
    rec {-exp-} where
    rec exp = case exp of
                    Const value ->
                        \env -> Value value

                    Fun (Fn _ f _ _) ->
                        \env -> Function (\p -> Value (f (value_ p)))

                    Var name ->
                        let index = getOrFail (elemIndex name nameStack) in
                        \env -> env !! index

                    App fun arg ->
                        let fun0 = rec fun in
                        let arg0 = rec arg in
                        \env -> function_ (fun0 env) (arg0 env)

                    Lambda var body ->
                        let body0 = createCompiler (varName var : nameStack) body in
                        \env -> Function (\arg -> body0 (arg : env))

eval :: Expression a -> Primitive a
eval input = createCompiler [] input []
