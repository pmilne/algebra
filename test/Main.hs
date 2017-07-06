module Main where

import           Numeral.TestNumeral1
import           TestComplex
import           TestExpression

{-
stack build && .stack-work/install/x86_64-osx/lts-8.15/8.0.2/bin/algebra
cabal repl
:info Ratio
-}
import           TestFactorial
import           Lambda.TestLambda1
import           Lambda.TestLambda2
import           Lambda.TestLambda3
import           TestModular
import           TestPolynomial
import           TestRational
import           TestTypes

main :: IO ()
main = do
  TestFactorial.run
  Numeral.TestNumeral1.run
  TestComplex.run
  TestModular.run
  TestRational.run
  TestPolynomial.run
  TestTypes.run
  TestExpression.run
  Lambda.TestLambda1.run
  Lambda.TestLambda2.run
  Lambda.TestLambda3.run
