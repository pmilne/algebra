module TestFactorial where

import Factorial

import TestUtil

ff :: Int
--ff = 2 ^ 27
ff = 6

run :: IO ()
run = do
          putStrLn ("factorial1 " ++ show ff ++ " = " ++ show (factorial1 ff))
          putStrLn ("factorial2 " ++ show ff ++ " = " ++ show (factorial2 ff))
