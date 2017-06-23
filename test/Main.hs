module Main where

{-
stack build && .stack-work/install/x86_64-osx/lts-8.15/8.0.2/bin/algebra
cabal repl
:info Ratio
-}

import TestFactorial
import qualified Numeral.TestNumeral1 as TestNumeral1
import TestComplex
import TestModular
import TestRational
import TestExpression
import qualified Expression.TestEval as TestEval
import TestPolynomial
import TestTypes
import TestLambda
import TestLambda2
import TestLambda3

main :: IO ()
main = do
--          TestFactorial.run
--          TestNumeral1.run
--          TestComplex.run
--          TestModular.run
--          TestRational.run
--          TestPolynomial.run
--          TestTypes.run
--          TestExpression.run
          TestEval.run
--          TestLambda.run
--          TestLambda2.run
--          TestLambda3.run





