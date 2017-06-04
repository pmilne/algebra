module Collins where

import Prelude hiding ((-), rem, negate, (*), (^), (/))
import Additive
import Ring
import Euclidean
import Polynomial
import Debug.Trace

infixr 8 ^

powerAssociative :: (a -> a -> a) -> a -> a -> Integer -> a
powerAssociative _  a0 _ 0 = a0
powerAssociative op a0 a n =
--    trace ("powerAssociative: " ++ show n) $
    powerAssociative op (if even n then a0 else op a0 a) (op a a) (div n 2)

(^) :: Ring a => a -> Integer -> a
a ^ n = if n < 0 then error "Collins: Negative exponent" else powerAssociative (*) one a n

pseudoRem :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> a -> Integer -> Polynomial a
pseudoRem u v lcv delta = rem (Const (lcv ^ (delta Prelude.+ 1)) * u) v

-- degree u >= degree v
subresultant :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> a -> a  -> (Polynomial a -> a -> r) -> r
subresultant u v g h | trace ("subresultant "
                                            ++ "\t\tu: " ++ show u
                                            ++ "\t\tv: " ++ show v
                                            ++ "\t\tg: " ++ show g
                                            ++ "\t\th: " ++ show h
                                            ) False = undefined
subresultant u@(Const lcu) (Const lcv) g h =
       if lcv == zero then
--            // u is gcd.
            trace "v is zero returning [1]." $
            \f -> f u h
        else
            trace "v is non-zero constant, returning [2]." $
            \f -> f (Const h) zero
subresultant u (Const lcv) g h =
       if lcv == zero then
            trace "v is zero returning [3]." $
            \f -> f u zero
        else
            trace "v is non-zero constant, returning [4]." $
            let delta = degree u in
            let nh = divideOrFail (lcv ^ delta) (h ^ (delta - 1)) in
            trace ("nh = " ++ show nh) $
            \f -> f (Const nh) zero
subresultant u v@(Term lcv degv redV) g h =
            trace "u, v both non-constant [5]." $
            let delta = degree u - degv in
            let pRem = pseudoRem u v lcv delta in
            let nh = divideOrFail (lcv ^ delta) (h ^ (delta - 1)) in
            subresultant v (divideOrFail pRem (Const (g * (h ^ delta)))) lcv nh

gcdAndResultant :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> (Polynomial a -> a  -> r) -> r
gcdAndResultant u v =
        if degree u > degree v then subresultant u v one one else subresultant v u one one

resultant :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> a
resultant u v = gcdAndResultant u v (\g r -> r)

gcd :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> Polynomial a
gcd u v = gcdAndResultant u v (\g r -> g)

