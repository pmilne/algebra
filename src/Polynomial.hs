module Polynomial where

import Prelude hiding ((+), (-), negate, (*), (^), (/))
import Ring
import Euclidean
import Debug.Trace

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
polynomial a n r = if a == zero then r else Term a n r

scaleAndShift :: (Show a, Eq a, Ring a) => a -> Integer -> Polynomial a -> Polynomial a
scaleAndShift a1 n1 (Const a2)      = if n1 == 0 then Const (a1 * a2) else polynomial (a1 * a2) n1 zero
scaleAndShift a1 n1 (Term a2 n2 r2) = polynomial (a1 * a2) (n1 + n2) (scaleAndShift a1 n1 r2)

promote :: a -> Polynomial a
promote = Const

promote1 :: Additive a => a -> Integer -> Polynomial a
promote1 a n = Term a n (Const zero)

instance (Show a, Eq a, Multiplicative a) => Show (Polynomial a) where
    show (Const a)      = show a
    show (Term a n r)   = (if a /= one then show a ++ " * " else "") ++ "x^" ++ show n ++ " + " ++ show r

instance (Show a, Eq a, Additive a) => Additive (Polynomial a) where
    Const a1      + Const a2                = Const (a1 + a2)
    Const a1      + Term a2 n2 r2           = Term a2 n2 (Const a1 + r2)
    Term a1 n1 r1 + Const a2                = Term a1 n1 (r1 + Const a2)
    p1@(Term a1 n1 r1) + p2@(Term a2 n2 r2) | n1 > n2   = Term a1 n1 (r1 + p2)
                                            | n1 < n2   = Term a2 n2 (p1 + r2)
                                            | otherwise = polynomial (a1 + a2) n1 (r1 + r2)
    zero                     = Const zero

instance (Show a, Eq a, Ring a) => Multiplicative (Polynomial a) where
    Const a1      * Const a2      = Const (a1 * a2)
    Const a1      * Term a2 n2 r2 = polynomial (a1 * a2) n2 (Const a1 * r2)
    Term a1 n1 r1 * Const a2      = polynomial (a1 * a2) n1 (r1 * Const a2)
    Term a1 n1 r1 * p2            = scaleAndShift a1 n1 p2 + r1 * p2;
    one                      = Const one

instance (Show a, Eq a, Ring a) => Subtractive (Polynomial a) where
    neg (Term a n r)      = Term (neg a) n (neg r)
    neg (Const a)         = Const (neg a)

instance (Show a, Eq a, Ring a) => Ring (Polynomial a) where

quoAndRem :: a -> b -> (a -> b -> c) -> c
quoAndRem q r f = f q r

quo1 q r = q
rem1 q r = r

divide :: (Show a, Eq a, Ring a, Euclidean a) => (Polynomial a -> Polynomial a -> b) -> Polynomial a -> Polynomial a -> b
--divide f u v | trace ("divide " ++ show u ++ " " ++ show v) False = undefined
divide f u v =
        if v == zero then error "Divide by zero " else
        let delta = deg u - deg v in
        if u == zero || delta < 0 then f zero u else
        let r = divideOrFail (lc u) (lc v) in
        let quoAndRem1 = divide quoAndRem (red u - scaleAndShift r delta (red v)) v in
        f (Term r delta (quoAndRem1 quo1)) (quoAndRem1 rem1)

instance (Show a, Eq a, Ring a, Euclidean a) => Euclidean (Polynomial a) where
    quo                = divide quo1
    rem                = divide rem1
    gcd               = undefined -- Collins.gcd
    -- Divide by an element of the coefficient domain.
    divideOrFail (Const c)    (Const v) = Const (divideOrFail c v)
    divideOrFail (Term a n r) (Const v) = Term  (divideOrFail a v) n (divideOrFail r (Const v))
    canonical n d = undefined -- let g = Prelude.signum d * Prelude.gcd n d in \f -> f (Prelude.quot n g) (Prelude.quot d g)

