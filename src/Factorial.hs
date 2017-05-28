module Factorial where

factorial1 :: Int -> Int
factorial1 0 = 1
factorial1 n = n * factorial1 (n - 1)

factorial2 :: Int -> Int
factorial2 n = if n == 1 then 1 else n * factorial2 (n - 1)

