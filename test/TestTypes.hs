module TestTypes (run) where

{-
Everything below this comment can be copy-pasted into: https://repl.it/languages/haskell

The on-line Haskell REPL doesn't seem to support selective imports of Prelude, so (+) has been replaced with (~+).
-}

infixl 6 ~+

class Additive a where
  (~+) :: a -> a -> a
  zero :: a

instance Additive Integer where
  (~+) = (Prelude.+)
  zero = 0

-- A data structure for Polynomials of the form a * x ^ n + r(x)
data Polynomial a = Const !a
                  | Term !a !Integer !(Polynomial a)
                  deriving (Eq, Read)

-- Constructor (with checks)
polynomial :: (Show a, Eq a, Additive a) => a -> Integer -> Polynomial a -> Polynomial a
polynomial a n r
  | a == zero = r
  | n == 0 = Const a
  | otherwise = Term a n r

instance (Show a, Eq a, Additive a) => Show (Polynomial a) where
    show (Const a)      = "Const " ++ show a
    show (Term a n r)   = show a ++ "x" ++ show n ++ " + " ++ show r

instance (Show a, Eq a, Additive a) => Additive (Polynomial a) where
    Const a1      ~+ Const a2                = Const (a1 ~+ a2)
    Const a1      ~+ Term a2 n2 r2           = Term a2 n2 (Const a1 ~+ r2)
    Term a1 n1 r1 ~+ Const a2                = Term a1 n1 (r1 ~+ Const a2)
    p1@(Term a1 n1 r1) ~+ p2@(Term a2 n2 r2) | n1 > n2   = Term a1 n1 (r1 ~+ p2)
                                             | n1 < n2   = Term a2 n2 (p1 ~+ r2)
                                             | otherwise = polynomial (a1 ~+ a2) n1 (r1 ~+ r2)
    zero                                     = Const zero

foo0 :: (Additive a) => a -> a
foo0 _ = zero

foo1 :: (Additive a) => Polynomial a -> a
foo1 _ = zero

foo2 :: (Additive a) => Polynomial (Polynomial a) -> a
foo2 _ = zero

run :: IO ()
run = do
  print (foo0 (undefined :: Integer))
  print (foo0 (undefined :: Polynomial Integer))
  print (foo0 (undefined :: Polynomial (Polynomial Integer)))
  print (foo0 (undefined :: Polynomial (Polynomial Integer)))
  print (foo1 (undefined :: Polynomial (Polynomial Integer)))
  print (foo2 (undefined :: Polynomial (Polynomial Integer)))

{-
Output:

0
Const 0
Const Const 0
Const Const 0
Const 0
0
-}

main :: IO ()
main = run
