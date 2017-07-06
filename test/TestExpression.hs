module TestExpression where

import           Expression.TestDerivative
import           Expression.TestEval
import           Expression.TestInverse

run :: IO ()
run = do
  Expression.TestEval.run
  Expression.TestDerivative.run
  Expression.TestInverse.run
