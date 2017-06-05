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
subresultant :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> a -> a  -> (Polynomial a -> a -> r) -> r
subresultant u v g h | trace ("subresultant "
                                            ++ "\t\tu: " ++ show u
                                            ++ "\t\tv: " ++ show v
                                            ++ "\t\tg: " ++ show g
                                            ++ "\t\th: " ++ show h
                                            ) False = undefined
subresultant u v g h =
       if v == zero then
--            trace "v is zero returning [3]." $
            \f -> f u zero
        else
--            trace "u, v both non-constant [5]." $
            let delta = deg u - deg v in
            let lcv = lc v in
            let nh = divideOrFail (lcv ^ delta) (h ^ (delta - 1)) in
            if deg v == 0 then
                \f -> f one nh
            else
                subresultant v (divideOrFail (pseudoRem u v lcv delta) (Const (g * (h ^ delta)))) lcv nh

gcdAndResultant :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> (Polynomial a -> a  -> r) -> r
gcdAndResultant u v =
        if deg u > deg v then subresultant u v one one else subresultant v u one one

resultant :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> a
resultant u v = gcdAndResultant u v (\g r -> r)

gcd1 :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> Polynomial a
gcd1 u v = gcdAndResultant u v (\g r -> g) -- this needs to separate pp and content etc

content :: (Euclidean a) => Polynomial a -> a
content = map2 (\ a _ r -> gcd a r) id

pp :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a
pp p = divideOrFail p (Const (content p))

instance (Show a, Eq a, Ring a, Euclidean a) => Euclidean (Polynomial a) where
    quo                = divide quo1
    rem                = divide rem1
    gcd u v            = let d = gcd (content u) (content v) in scale d (pp (Collins.gcd1 (pp u) (pp v)))
    divideOrFail p (Const v) = map1 (\a -> (divideOrFail a v)) p
    canonical n d = undefined -- let g = Prelude.signum d * Prelude.gcd n d in \f -> f (Prelude.quot n g) (Prelude.quot d g)
