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

toInt :: Primitive -> Int
toInt (Int0 i) = i
toInt p = undefined

getOrFail :: Maybe a -> a
getOrFail (Just x) = x
getOrFail Nothing = error "This didin't happen"

createCompiler :: [String] -> Expression -> [Primitive] -> Primitive
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
                        \env -> function_ (toFunction (fun0 env)) (arg0 env)

                    Lambda var exp ->
                        let var0 = varName var in
                        let exp0 = createCompiler (var0 : nameStack) exp in
                        \env -> Fun0 (Fun "hello" (\arg -> exp0 (arg : env)))


eval :: Expression -> Primitive
eval input = createCompiler [] input []
