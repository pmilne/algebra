module TestLambda2 where

import Prelude hiding (id)
import Lambda2

--import TestUtil

f, x, c0, c1, c2, id1 :: Expression Int
x = Symbol "x"
f = Symbol "f"
c0 = Lambda f (Lambda x x)
c1 = Lambda x x
c2 = Lambda f (Lambda x (Application f (Application f x)))
z = Constant (Val0 0)
inc = Constant (Fun0 (Fun "inc" (\p -> Val0 (1 + (toInt p)))))
id1 = Application c1 z

run :: IO ()
run = do
          putStrLn (show (eval c1))
          putStrLn (show (eval id1))

          putStrLn (show (eval (Application (Application c2 inc) z)))
          putStrLn (show (eval (Application (Application (Application c2 c2) inc) z)))
          putStrLn (show (eval (Application (Application (Application (Application c2 c2) c2) inc) z)))
          putStrLn (show (eval (Application (Application (Application (Application (Application c2 c2) c2) c2) inc) z)))
--          putStrLn (show (eval (Application (Application (Application (Application (Application c2 c2) c2) (Application c2 c2)) inc) z)))

