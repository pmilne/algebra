module TestExpression where

import Expression.TestEval
import Expression.TestDerivative
import Expression.TestInverse

run :: IO ()
run = do
          Expression.TestEval.run
          Expression.TestDerivative.run
          Expression.TestInverse.run

