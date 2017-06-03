module Polynomial where

import Prelude hiding ((+), (-), negate, (*), (^), (/))
import Additive
import Ring
--import Euclidean

data Polynomial a = Const !a
                  | Term !a !Integer !(Polynomial a)
                  deriving (Eq, Read)

scaleAndShift :: (Ring a) => a -> Integer -> Polynomial a -> Polynomial a
scaleAndShift a1 n1 (Const a2)      = Term (a1 * a2) n1 zero
scaleAndShift a1 n1 (Term a2 n2 r2) = Term (a1 * a2) (n1 + n2) (scaleAndShift a1 n1 r2)

instance (Show a) => Show (Polynomial a) where
    show (Const a)      = show a
    show (Term a n r)   = show a ++ " * x ^ " ++ show n ++ " + " ++ show r

instance (Additive a) => Additive (Polynomial a) where
    Const a1      + Const a2                = Const (a1 + a2)
    Const a1      + Term a2 n2 r2           = Term a2 n2 (Const a1 + r2)
    Term a1 n1 r1 + Const a2                = Term a1 n1 (r1 + Const a2)
    p1@(Term a1 n1 r1) + p2@(Term a2 n2 r2) | n1 > n2   = Term a1 n1 (r1 + p2)
                                            | n1 < n2   = Term a2 n2 (p1 + r2)
                                            | otherwise = Term (a1 + a2) n1 (r1 + r2)
    zero                     = Const zero

instance (Ring a) => Ring (Polynomial a) where
    Const a1      * Const a2      = Const (a1 * a2)
    Const a1      * Term a2 n2 r2 = Term (a1 * a2) n2 (Const a1 * r2)
    Term a1 n1 r1 * Const a2      = Term (a1 * a2) n1 (r1 * Const a2)
    Term a1 n1 r1 * p2            = scaleAndShift a1 n1 p2 + r1 * p2;

    negate (Term a n r)      = Term (negate a) n (negate r)
    negate (Const a)         = Const (negate a)

    one                      = Const zero

--instance (Ring a) => Euclidean (Polynomial a) where
--    canonical n d = let g = Prelude.signum d * Prelude.gcd n d in \f -> f (Prelude.quot n g) (Prelude.quot d g)

