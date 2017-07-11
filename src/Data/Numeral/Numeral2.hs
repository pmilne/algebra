{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Numeral.Numeral2 where

import           Domains.Ring
import           Exponentiative

type Numeral2 a = (a -> a) -> (a -> a)

instance Ring (Numeral2 a) where
  (+) = plus1
  (*) = times1
  negate _ = undefined
  zero = church 0
  one = church 1

--instance Exponentiable (Numeral2 a) where
--    a ^ b   = b a
church :: Integer -> Numeral2 a
church 0 = \_ x -> x
church n = \f x -> f (church (n Prelude.- 1) f x)

unchurch :: Numeral2 Integer -> Integer
unchurch n = n (Prelude.+ 1) 0

s :: Numeral2 a -> Numeral2 a
s n f x = f (n f x)

--plus1 :: (t3 -> t2 -> t1) -> (t3 -> t -> t2) -> t3 -> t -> t1
--plus1 :: (t3 -> t2 -> t1) -> (t3 -> t -> t2) -> t3 -> t -> t1
plus1 :: Numeral2 t -> Numeral2 t -> Numeral2 t
plus1 a b f x = a f (b f x)

--times1 :: (t2 -> t1) -> (t -> t2) -> t -> t1
--times1 :: (t -> t) -> (t -> t) -> (t -> t)
--times1 :: Numeral (t -> t) -> Numeral (t -> t) -> Numeral (t -> t)
--times1 :: Numeral (t2 -> t1) -> Numeral (t -> t2) -> Numeral (t -> t1)
times1 :: Numeral2 t -> Numeral2 t -> Numeral2 t
times1 a b f = a (b f)

--expt1 :: t1 -> (t1 -> t) -> t
--expt1 :: t -> (t -> t) -> t
expt1 :: Numeral2 t -> Numeral2 (t -> t) -> Numeral2 t
expt1 a b = b a
