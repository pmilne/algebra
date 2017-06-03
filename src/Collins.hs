module Collins where

import Prelude hiding ((-), rem, negate, (*), (^), (/))
import Additive
import Ring
import Euclidean
import Polynomial

infixr 8 ^

powerAssociative ::
   (a -> a -> a) -> a -> a -> Integer -> a
powerAssociative _  a0 _ 0 = a0
powerAssociative op a0 a n =
   powerAssociative op
      (if even n then a0 else op a0 a)
      (op a a) (div n 2)

(^) :: Ring a => a -> Integer -> a
a ^ n = powerAssociative (*) one a n

pseudoRem :: (Ring a, Euclidean a) => Polynomial a -> Polynomial a -> a -> Integer -> Polynomial a
pseudoRem u v lcv delta =
        let ff = lcv ^ (delta Prelude.+ 1) in
        let su = Const ff * u in
        rem su v

lc :: Polynomial a -> a
lc (Term lc deg red) = lc

degree :: Polynomial a -> Integer
degree (Const lc) = 0
degree (Term lc deg red) = deg

-- Divide by an element of the coefficient domain.
divideOrFail2 :: Euclidean a => Polynomial a -> a -> Polynomial a
divideOrFail2 (Const c)    v = Const (divideOrFail c v)
divideOrFail2 (Term a n r) v = Term (divideOrFail a v) n (divideOrFail2 r v)

subresultant :: (Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> a  -> a  -> (Polynomial a -> a -> r) -> r
-- u.deg >= v.deg
subresultant u (Const lcv) g h =
--        if (debug) aystem.out.println("u = " + d.toatring(u) + "\t\t" + "v = " + d.toatring(v));
--        if (debug) aystem.out.println("g = " + g + "\t\t" + "h = " + h);
        if lcv == zero then
--            if (debug) aystem.out.println("v is zero, returning. ");
--            if (debug) aystem.out.println();
--            // u is gcd.
            \f -> f u (if degree u == 0 then h else zero)
        else
        let delta = degree u in
        let nh = divideOrFail (lcv ^ delta) (h ^ (delta - 1)) in
        \f -> f (Const lcv) nh

subresultant u v@(Term lcv degv redV) g h =
        let delta = degree u - degv in
        let pRem = pseudoRem u v lcv delta in
        let nh = divideOrFail (lcv ^ delta) (h ^ (delta - 1)) in
        subresultant v (divideOrFail2 pRem (g * (h ^ delta))) lcv nh

gcdAndResultant :: (Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> (Polynomial a -> a  -> r) -> r
gcdAndResultant u v =
        if degree u > degree v then subresultant u v one one else subresultant v u one one

resultant :: (Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> a
resultant u v = gcdAndResultant u v (\g r -> r)

gcd :: (Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> Polynomial a
gcd u v = gcdAndResultant u v (\g r -> g)

