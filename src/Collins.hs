module Collins where

--import Prelude(Show, Eq, Integer)
import           Prelude           hiding (gcd, negate, rem, (*), (+), (-), (/), (^))

import           Domains.Euclidean
import           Domains.Ring

import           Debug.Trace
import           Polynomial

infixr 8 ^

powerAssociative :: (a -> a -> a) -> a -> a -> Integer -> a
powerAssociative _  a0 _ 0 = a0
powerAssociative op a0 a n =
--    trace ("powerAssociative: " ++ show n) $
    powerAssociative op (if even n then a0 else op a0 a) (op a a) (div n 2)

(^) :: Multiplicative a => a -> Integer -> a
a ^ n = if n < 0 then error "Collins: Negative exponent" else powerAssociative (*) one a n

pseudoRem :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> Polynomial a
pseudoRem u v = rem (Const (lc v ^ (deg u - deg v + 1)) * u) v

_gcd :: a -> b -> a
_gcd g _ = g

_resultant :: a -> b -> b
_resultant _ r = r

-- Translated from Knuth Volume II: "The Subresultant Algorithm", Section 4.6.1
-- subresultant returns (rtn gcd resultant) where gcd = gcd(u, v) and resultant = resultant(u, v).
subresultant :: (Show a, Eq a, Ring a, Euclidean a) => (Polynomial a -> a -> r) -> Polynomial a -> Polynomial a -> r
subresultant rtn u v =
        (if deg u > deg v then rec u v else rec v u) one one where -- avoid passing the rtn function in the recursion
        -- degree u >= degree v
        rec u v g h =
             trace ("subresultant " ++ "\t\tu: " ++ show u ++ "\t\tv: " ++ show v ++ "\t\tv: " ++ show g ++ "\t\tv: " ++ show h) $
             if v == zero then
                rtn u zero
             else
               let delta = deg u - deg v in
               let lcv = lc v in
               -- The first clause is only required in the first iteration where delta may be zero, requiring an explicit check.
               let nh = if delta == 0 && h == one then lcv else (lcv ^ delta) /! (h ^ (delta - 1)) in
               if deg v == 0 then
                   rtn one nh
               else
                   rec v (pseudoRem u v /! Const (g * (h ^ delta))) lcv nh

resultant :: (Show a, Eq a, Ring a, Euclidean a) => Polynomial a -> Polynomial a -> a
resultant = subresultant _resultant

gcd1 u v = let g = subresultant _gcd u v in sign g * g

instance (Show a, Eq a, Ring a, Euclidean a) => Euclidean (Polynomial a) where
    divide          = divide1
    gcd u v         = let d = gcd (content u) (content v) in Const d * pp (gcd1 (pp u) (pp v))
    sign p          = Const (sign (lc p))
