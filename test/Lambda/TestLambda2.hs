module Lambda.TestLambda2 where

import Prelude hiding (id)

import Lambda.Lambda2

f, x, c0, c1, c2, id1, z, inc :: Expression Int
x = Symbol "x"
f = Symbol "f"
c0 = Lambda "f" (Lambda "x" x)
c1 = Lambda "x" x
c2 = Lambda "f" (Lambda "x" (Application f (Application f x)))
z = Constant (Value 0)
inc = Constant (Function (\p -> Value (1 + value_ p)))
id1 = Application c1 z

run :: IO ()
run = do
          print (eval c1)
          print (eval id1)

          print (eval (Application (Application c2 inc) z))
          print (eval (Application (Application (Application c2 c2) inc) z))
          print (eval (Application (Application (Application (Application c2 c2) c2) inc) z))
          print (eval (Application (Application (Application (Application (Application c2 c2) c2) c2) inc) z))
--          print (eval (Application (Application (Application (Application (Application c2 c2) c2) (Application c2 c2)) inc) z))

