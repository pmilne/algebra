module Lambda.TestLambda3 where

import Prelude hiding (id)

import Lambda.Lambda3

f, x, c0, c1, c2, id1, z, inc :: Expression Int
x = Symbol "x"
f = Symbol "f"
c0 = Lambda "f" (Lambda "x" x)
c1 = Lambda "x" x
c2 = Lambda "f" (Lambda "x" (Application f (Application f x)))
z = Constant 0
inc = Fun (Fn "inc" (+1))
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

