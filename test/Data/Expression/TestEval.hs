module Data.Expression.TestEval where

import Prelude hiding (id, sin)

import Domains.Trigonometric

import Data.Expression
import Data.Expression.Eval

f, x, c0, c1, c2, id1, z, inc :: Expression Int
x = Var "x"
f = Var "f"
c0 = Lambda f (Lambda x x)
c1 = Lambda x x
c2 = Lambda f (Lambda x (App f (App f x)))
z = Const 0
inc = Fun (Fn "inc" (+1))
id1 = App c1 z

d :: Expression Double
d = Var "x"

run :: IO ()
run = do
          putStrLn ("sin (1) = " ++ show (eval (App (Lambda d (sin d)) (Const 1.0))))

          print (eval c1)
          print (eval id1)

          print (eval (App (App c2 inc) z))
          print (eval (App (App (App c2 c2) inc) z))
          print (eval (App (App (App (App c2 c2) c2) inc) z))
          print (eval (App (App (App (App (App c2 c2) c2) c2) inc) z))
--          print (eval (App (App (App (App (App c2 c2) c2) (App c2 c2)) inc) z))

