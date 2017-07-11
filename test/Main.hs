module Main where

import           Data.Numeral.TestNumeral1
import           Data.TestComplex
import           Data.TestExpression

{-
stack build && .stack-work/install/x86_64-osx/lts-8.15/8.0.2/bin/algebra
cabal repl
:info Ratio
-}
import           Data.TestModular
import           Data.TestPolynomial
import           Data.TestRational

import           Lambda.TestLambda1
import           Lambda.TestLambda2
import           Lambda.TestLambda3

import           TestFactorial
import           TestTypes

main :: IO ()
main = do
  TestFactorial.run
  TestTypes.run

  Data.TestComplex.run
  Data.TestModular.run
  Data.TestRational.run
  Data.TestPolynomial.run
  Data.TestExpression.run
  Data.Numeral.TestNumeral1.run

  Lambda.TestLambda1.run
  Lambda.TestLambda2.run
  Lambda.TestLambda3.run
