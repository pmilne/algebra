module Main where

import           Expression.TestEval
import           Numeral.TestNumeral1
import           TestComplex
import           TestExpression

{-
stack build && .stack-work/install/x86_64-osx/lts-8.15/8.0.2/bin/algebra
cabal repl
:info Ratio
-}
import           TestFactorial
import           TestLambda
import           TestLambda2
import           TestLambda3
import           TestModular
import           TestPolynomial
import           TestRational
import           TestTypes

main :: IO ()
main = do
  TestFactorial.run
  TestNumeral1.run
  TestComplex.run
  TestModular.run
  TestRational.run
  TestPolynomial.run
  TestTypes.run
  TestExpression.run
  TestLambda.run
  TestLambda2.run
  TestLambda3.run
