module Polynomial where

import Prelude hiding ((+), (-), negate, (*), (^), (/), gcd)
import Domains.Ring
import Domains.Euclidean
import Debug.Trace

-- A data structure for Polynomials of the form a * x ^ n + r(x)
data Polynomial a = Const !a
                  | Term !a !Integer !(Polynomial a)
                  deriving (Eq, Read)

lc :: Polynomial a -> a
lc (Const a) = a
lc (Term a _ _) = a

deg :: Polynomial a -> Integer
deg (Const _) = 0
deg (Term _ n _) = n

red :: (Show a, Eq a, Additive a) => Polynomial a -> Polynomial a
red (Const _) = zero
red (Term _ _ r) = r

polynomial :: (Show a, Eq a, Additive a) => a -> Integer -> Polynomial a -> Polynomial a
--polynomial a n r | trace ("polynomial " ++ show a ++ " " ++ show n) False = undefined
polynomial a n r = if a == zero then r else if n == 0 then Const a else Term a n r

map2 :: (a -> Integer -> b -> b) -> (a -> b) -> Polynomial a -> b
map2 f g (Term a n r) = f a n (map2 f g r)
map2 f g (Const c)    = g c

map1 :: (a -> b) -> Polynomial a -> Polynomial b
map1 f = map2 (\a -> Term (f a)) (\a -> Const (f a)) -- note currying of constructors

scale :: (Show a, Eq a, Ring a) => a -> Polynomial a -> Polynomial a
scale k = map1 (k *)

divideOrFail2 :: Euclidean b => Polynomial b -> b -> Polynomial b
divideOrFail2 p v = map1 (\a -> divideOrFail a v) p

scaleAndShift :: (Show a, Eq a, Ring a) => a -> Integer -> Polynomial a -> Polynomial a
scaleAndShift k delta = if delta == 0 then scale k else
            map2 (\a n -> polynomial (k * a) (n + delta)) (\a -> polynomial (k * a) delta zero) -- note currying

promote :: a -> Polynomial a
promote = Const

promote1 :: Additive a => a -> Integer -> Polynomial a
promote1 a n = Term a n (Const zero)

instance (Show a, Eq a, Additive a, Multiplicative a) => Show (Polynomial a) where
--    show (Const a)      = "Const " ++ show a
    show (Const a)      = show a
    show (Term a n r)   = (if a /= one then show a ++ " * "  else "") ++ "x" ++
                          (if n /= one then "^"    ++ show n else "") ++ " + " ++
                          (if r /= zero then show r else "")

instance (Show a, Eq a, Additive a) => Additive (Polynomial a) where
    Const a1      + Const a2                = Const (a1 + a2)
    Const a1      + Term a2 n2 r2           = Term a2 n2 (Const a1 + r2)
    Term a1 n1 r1 + Const a2                = Term a1 n1 (r1 + Const a2)
    p1@(Term a1 n1 r1) + p2@(Term a2 n2 r2) | n1 > n2   = Term a1 n1 (r1 + p2)
                                            | n1 < n2   = Term a2 n2 (p1 + r2)
                                            | otherwise = polynomial (a1 + a2) n1 (r1 + r2)
    zero                                    = Const zero

instance (Negatable a) => Negatable (Polynomial a) where
    neg       = map1 neg

instance (Show a, Eq a, Subtractive a) => Subtractive (Polynomial a) where

instance (Show a, Eq a, Ring a) => Multiplicative (Polynomial a) where
    Const a1      * Const a2      = Const (a1 * a2)
    Const a1      * Term a2 n2 r2 = polynomial (a1 * a2) n2 (Const a1 * r2)
    Term a1 n1 r1 * Const a2      = polynomial (a1 * a2) n1 (r1 * Const a2)
    Term a1 n1 r1 * p2            = scaleAndShift a1 n1 p2 + r1 * p2;
    one                           = Const one

instance (Show a, Eq a, Ring a) => Ring (Polynomial a) where

quoAndRem :: a -> b -> (a -> b -> c) -> c
quoAndRem q r f = f q r

divide1 :: (Show a, Eq a, Ring a, Euclidean a) => (Polynomial a -> Polynomial a -> b) -> Polynomial a -> Polynomial a -> b
--divide f u v | trace ("divide " ++ show u ++ " " ++ show v) False = undefined
divide1 rtn u v =
        if v == zero then error "Polynomial:: divide by zero. " else
        let delta = deg u - deg v in
        if u == zero || delta < 0 then rtn zero u else
        let r = divideOrFail (lc u) (lc v) in
        let quoAndRem1 = divide1 quoAndRem (red u - scaleAndShift r delta (red v)) v in
--        trace ("quo = " ++ show (quoAndRem1 _quo))
--        trace ("rem = " ++ show (quoAndRem1 _rem))
        rtn (polynomial r delta (quoAndRem1 _quo)) (quoAndRem1 _rem)

content :: (Euclidean a) => Polynomial a -> a
content = map2 (\ a _ r -> gcd a r) id

pp :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a
pp p = divideOrFail2 p (content p)

