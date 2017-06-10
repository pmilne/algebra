module Collins where

--import Prelude(Show, Eq, Integer)
import Prelude hiding ((+), (-), negate, (*), (^), (/), rem, gcd)
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
pseudoRem u v lcv delta = rem (Const (lcv ^ (delta + 1)) * u) v

-- Translated from Knuth Volume II: "The Subresultant Algorithm", Section 4.6.1
-- degree u >= degree v
subresultant :: (Show a, Eq a, Ring a, Euclidean a) => (Polynomial a -> a -> r) -> Polynomial a -> Polynomial a -> r
subresultant rtn u0 v0 =
        rec u0 v0 one one where -- avoid passing the rtn function in the recursion
        rec u v g h =
             trace ("subresultant " ++ "\t\tu: " ++ show u ++ "\t\tv: " ++ show v ++ "\t\tv: " ++ show g ++ "\t\tv: " ++ show h) $
             if v == zero then
                rtn u zero
             else
               let delta = deg u - deg v in
               let lcv = lc v in
               let nh = divideOrFail (lcv ^ delta) (h ^ (delta - 1)) in -- todo not valid if delta == 0
               if deg v == 0 then
                   rtn one nh
               else
--                subresultant f v (divideOrFail (pseudoRem u v lcv delta) (Const (g * (h ^ delta)))) lcv nh -- also works
                  rec v (divideOrFail2 (pseudoRem u v lcv delta) (g * (h ^ delta))) lcv nh

gcdAndResultant :: (Show a, Eq a, Ring a, Euclidean a) => (Polynomial a -> a -> r) -> Polynomial a -> Polynomial a -> r
gcdAndResultant rtn u v = if deg u > deg v then subresultant rtn u v else subresultant rtn v u

resultant :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> a
resultant = gcdAndResultant (\_ r -> r)

gcd1 :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> Polynomial a
gcd1 = gcdAndResultant (\g _ -> g)

instance (Show a, Eq a, Ring a, Euclidean a) => Euclidean (Polynomial a) where
    divide          = divide1
    gcd u v         = let d = gcd (content u) (content v) in scale d (pp (gcd1 (pp u) (pp v)))
    sign p          = Const (sign (lc p))
