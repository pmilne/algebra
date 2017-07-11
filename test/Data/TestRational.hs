module Data.TestRational where

import           Prelude       hiding (Rational, gcd, negate, (*), (+), (-), (/), (^))

import           Domains.Field
import           Domains.Ring

import           Data.Complex
import           Data.Rational

import           TestUtil

fr :: Rational Int
fr = Rational 1 2

run :: IO ()
run = do
  putStrLn ("(1 / 2) = " ++ show fr)
