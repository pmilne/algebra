module Collins where

import Prelude hiding ((-), rem, negate, (*), (^), (/), gcd)
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

(^) :: Multiplicative a => a -> Integer -> a
a ^ n = if n < 0 then error "Collins: Negative exponent" else powerAssociative (*) one a n

pseudoRem :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> a -> Integer -> Polynomial a
pseudoRem u v lcv delta = rem (Const (lcv ^ (delta Prelude.+ 1)) * u) v

-- degree u >= degree v
subresultant :: (Show a, Eq a, Ring a, Euclidean a) => (Polynomial a -> a -> r) -> Polynomial a -> Polynomial a -> a -> a -> r
subresultant f u v g h | trace ("subresultant "
                                            ++ "\t\tu: " ++ show u
                                            ++ "\t\tv: " ++ show v
                                            ++ "\t\tg: " ++ show g
                                            ++ "\t\th: " ++ show h
                                            ) False = undefined
subresultant f u v g h =
       if v == zero then
            f u zero
        else
            let delta = deg u - deg v in
            let lcv = lc v in
            let nh = divideOrFail (lcv ^ delta) (h ^ (delta - 1)) in
            if deg v == 0 then
                f one nh
            else
                subresultant f v (divideOrFail2 (pseudoRem u v lcv delta) (g * (h ^ delta))) lcv nh

gcdAndResultant :: (Show a, Eq a, Ring a, Euclidean a) => (Polynomial a -> a  -> r) -> Polynomial a -> Polynomial a -> r
gcdAndResultant f u v = (if deg u > deg v then subresultant f u v else subresultant f v u) one one

resultant :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> a
resultant = gcdAndResultant (\_ r -> r)

gcd1 :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> Polynomial a
gcd1 = gcdAndResultant (\g _ -> g)

instance (Show a, Eq a, Ring a, Euclidean a) => Euclidean (Polynomial a) where
    quo             = divide quo1
    rem             = divide rem1
    gcd u v         = let d = gcd (content u) (content v) in scale d (pp (gcd1 (pp u) (pp v)))
    divideOrFail    = divide (\q r -> if r /= zero then error "Collins:: divideOrFail" else q)
    sign p          = Const (sign (lc p))
    canonical f n d = let g = sign d * gcd n d in f (divideOrFail n g) (divideOrFail d g)
