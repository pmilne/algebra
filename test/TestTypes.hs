module TestTypes where

import Ring

import Polynomial

import TestUtil

pp0 :: Polynomial (Polynomial (Polynomial (Polynomial Integer)))
pp0 = zero

foo :: (Multiplicative a) => a -> a
foo x = one

bar :: (Multiplicative a) => Polynomial a -> a
bar x = one

baz :: (Multiplicative a) => Polynomial (Polynomial a) -> a
baz x = one

run :: IO ()
run = do
          putStrLn ("zero = " ++ show (foo (undefined :: Polynomial (Polynomial (Polynomial (Polynomial Integer))))))
          putStrLn ("zero = " ++ show (foo (undefined :: Polynomial (Polynomial Integer))))
          putStrLn ("zero = " ++ show (bar (undefined :: Polynomial (Polynomial Integer))))
          putStrLn ("zero = " ++ show (foo pp0))
          putStrLn ("zero = " ++ show (bar pp0))
          putStrLn ("zero = " ++ show (baz pp0))

