module Numeral where

import Ring

-- The Church Numeral type
newtype Numeral t
  = Numeral ((t -> t) -> (t -> t))
--  = Numeral1 (Int -> Int)
--  = Numeral1 ((t -> t) -> (t -> t))
--        deriving (Eq, Show, Read)

--cinc :: Integer -> Integer
--cinc x = x + 1

--unchurch :: ((s -> s) -> (s -> s)) -> Integer
--unchurch :: ((a -> a) -> a -> a) -> Integer
--unchurch n = n cinc (0 :: Integer)


--unchurch2 :: ((Integer -> Integer) -> t) -> t
--unchurch2 n = n cinc

--unchurch3 :: Num t => ((Integer -> Integer) -> t -> t1) -> t1
--unchurch3 :: ((s -> s) -> (s -> s)) -> Integer
--unchurch3 n = unchurch2 n (0 :: Integer)

--instance Show (Numeral1 s) where
--   show (Numeral1 n1) = show (unchurch3 n1)

instance (Show s, Num s) => Show (Numeral s) where
   show (Numeral n1) = show (n1 (Prelude.+1) 0)

--instance (Show a, Num a) => Show ((a -> a) -> a -> a) where
--    show n = show $ n (+1) 0

instance  (Num a) => Num (Numeral a) where
    (Numeral n1) + (Numeral n2) = Numeral (\f x -> (n1 f (n2 f x)))
    (Numeral n1) * (Numeral n2) = Numeral (\f -> n1 (n2 f))
    negate (Numeral n1)         =  undefined
    abs z                       =  undefined
    signum z                    =  undefined
    fromInteger n               =  undefined

instance  (Ring a) => Ring (Numeral a) where
    (Numeral n1) + (Numeral n2) = Numeral (\f x -> (n1 f (n2 f x)))
    (Numeral n1) * (Numeral n2) = Numeral (\f -> n1 (n2 f))
    negate (Numeral n1)         =  undefined
    zero                        = Numeral (church 0)
    one                        = Numeral (church 1)

type Numeral2 a = (a -> a) -> (a -> a)

church :: Integer -> Numeral2 a
church 0 = \ _ x -> x
church n = \ f x -> f (church (n Prelude.- 1) f x)

cinc :: Integer -> Integer
cinc x = x Prelude.+ 1

ident :: a -> a
ident x = x

unchurch :: Numeral2 Integer -> Integer
unchurch n = n cinc 0 :: Integer

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

{-
pow :: Numeral t -> Numeral t -> Numeral t
-- pow :: t -> (t -> u) -> u
pow a n = n a
-}

{-
pow :: Church a -> Church a -> Church a
pow a n = n

instance Show (Church a) where
	show x = show (unchurch x)

instance Show ((a -> a) -> a -> a) where
	show x = show (unchurch x)
-}

