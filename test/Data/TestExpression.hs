module Data.TestExpression where

import           Data.Expression.TestDerivative
import           Data.Expression.TestEval
import           Data.Expression.TestInverse

run :: IO ()
run = do
  Data.Expression.TestEval.run
  Data.Expression.TestDerivative.run
  Data.Expression.TestInverse.run
