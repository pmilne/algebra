{-
Originally from: http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
-}
module Expression where

import           Prelude        hiding (acos, asin, atan, cos, exp, log, negate, sin, sqrt, tan, (*), (+), (-), (/),
                                 (^))

import           Applicable
import           Exponentiative
import           Field
import           Trigonometric

--import           Debug.Trace
infixl 1 ~>

{-# ANN module "HLint: ignore Redundant bracket" #-}

{-# ANN module "HLint: ignore Eta reduce" #-}

{-# ANN module "HLint: ignore Avoid lambda" #-}

{-# ANN module "HLint: ignore Unused matches" #-}

data Fn a = Fn
  { name_ :: String
  , fun_  :: a -> a
  }

fnValue :: Expression a -> a -> a
fnValue (Fun f) = fun_ f
fnValue _       = undefined

instance Eq (Fn a) where
  Fn name1 _ == Fn name2 _ = name1 == name2

instance Show (Fn a) where
  show (Fn name1 _) = name1

data Expression a
  = Const !a
  | Var !String
  | Fun !(Fn a)
  | Lambda !(Expression a)
           !(Expression a)
  | App !(Expression a)
        !(Expression a)
  | Sum !(Expression a)
        !(Expression a)
  | Neg !(Expression a)
  | Prd !(Expression a)
        !(Expression a)
  | Inv !(Expression a)
  | Pow !(Expression a)
        !(Expression a)
  | Log !(Expression a)
        !(Expression a)
  deriving (Eq)

(~>) :: Expression a -> Expression a -> Expression a
x ~> body = Lambda x body

varName :: Expression a -> String
varName (Var s) = s
varName _       = error "Formal parameter to Lambda wasn't a symbol!"

instance (Show a) => Show (Expression a) where
  show (Const a)         = show a
  show (Var a)           = a
  show (Fun f)           = name_ f
  show (Lambda var body) = "(" ++ show var ++ " ~> " ++ show body ++ ")"
  show (App f a)         = "(" ++ show f ++ " " ++ show a ++ ")"
  show (Sum a b)         = "(" ++ show a ++ " + " ++ show b ++ ")"
  show (Neg a)           = "(" ++ "-" ++ show a ++ ")"
  show (Prd a b)         = "(" ++ show a ++ "*" ++ show b ++ ")"
  show (Pow a b)         = "(" ++ show a ++ "^" ++ show b ++ ")"
  show (Log a b)         = "(log (base " ++ show a ++ ") " ++ show b ++ ")"
  show (Inv a)           = "(" ++ "/" ++ show a ++ ")"

instance (Eq a, Additive a) => Additive (Expression a) where
  Const a + Const b = Const (a + b)
  Const a + b
    | a == zero = b
    | otherwise = Sum (Const a) b
  a + Const b
    | b == zero = a
    | otherwise = Sum a (Const b)
  a + b = Sum a b
  zero = Const zero

instance (Negatable a) => Negatable (Expression a) where
  neg (Const a) = Const (neg a)
  neg (Neg a)   = a
  neg a         = Neg a

instance (Eq a, Subtractive a) => Subtractive (Expression a)

instance (Eq a, Additive a, Multiplicative a) => Multiplicative (Expression a) where
  Const a * Const b = Const (a * b)
  a * Const b
    | b == zero = zero
    | b == one = a
    | otherwise = Prd (Const b) a -- if commutative we can move the constant to the front
  Const a * (Prd (Const b) c) = Prd (Const (a * b)) c -- if associative, gather constants
  Const a * b
    | a == zero = zero
    | a == one = b
    | otherwise = Prd (Const a) b
  (Inv a) * (Inv b) = Inv (a * b)
  a * (Prd b (Inv c)) = Prd (a * b) (Inv c)
  a * (Inv b)
    | a == b = one
    | otherwise = Prd a (Inv b)
  a * b
    | a == b = Pow a (Const one + Const one)
    | otherwise = Prd a b
  one = Const one

instance (Eq a, Ring a) => Ring (Expression a)

instance (Eq a, Field a) => Invertable (Expression a) where
  inv (Const a) = Const (inv a)
  inv a         = Inv a

instance (Eq a, Field a) => Field (Expression a) where
  a / b = a * inv b

instance (Eq a, Field a, Exponentiative a) => Exponentiative (Expression a) where
  Const a ^ Const b = Const (a ^ b)
  a ^ Const b
    | b == zero = one
    | b == one = a
    | otherwise = Pow a (Const b)
  Const a ^ b
    | a == zero = zero
    | a == one = one
    | otherwise = Pow (Const a) b
  a ^ b = Pow a b
  log a b
    | b == one = zero
    | otherwise = Log a b
  ln (Const a) = Const (ln a)
  ln a         = App (Fun (Fn "ln" ln)) a
  exp (Const a) = Const (exp a)
  exp a         = App (Fun (Fn "exp" exp)) a
  sqrt = App (Fun (Fn "sqrt" sqrt))
  two = Const two

-- https://en.wikipedia.org/wiki/Differentiation_of_trigonometric_functions
instance (Eq a, Field a, Exponentiative a, Trigonometric a) => Trigonometric (Expression a) where
  sin = App (Fun (Fn "sin" sin))
  cos = App (Fun (Fn "cos" cos))
  tan = App (Fun (Fn "tan" tan))
  asin = App (Fun (Fn "asin" asin))
  acos = App (Fun (Fn "acos" acos))
  atan = App (Fun (Fn "atan" atan))

instance (Show a, Eq a, Field a, Exponentiative a, Applicable a) => Applicable (Expression a) where
  apply (Lambda x body) arg = substitute (varName x) arg body
  apply f x                 = App f x

map0 ::
     (Show b, Eq b, Field b, Exponentiative b)
  => (String -> b)
  -> (a -> b)
  -> (Fn a -> b)
  -> (Expression a -> b -> b)
  -> Expression a
  -> b
map0 mapVar mapConst mapFun mapApplyFun e0 = rec e0
  where
    rec e =
      case e of
        Const a -> mapConst a
        Var a   -> mapVar a
        Fun f   -> mapFun f
        App f a -> mapApplyFun f (rec a)
        Neg a   -> neg (rec a)
        Sum a b -> rec a + rec b
        Prd a b -> rec a * rec b
        Inv a   -> inv (rec a)
        Pow a b -> rec a ^ rec b
        Log a b -> log (rec a) (rec b)

--  trace ("evalExpr: " ++ nm ++ " -> " ++ show val ++ " in " ++ show exp) $
eval1 :: (Show a, Eq a, Field a, Exponentiative a, Applicable a) => String -> a -> Expression a -> a
eval1 name value exp0 =
  map0
    (\vName ->
       if name == vName
         then value
         else undefined)
    id
    undefined
    fnValue
    exp0

substitute ::
     (Applicable a, Exponentiative a, Field a, Eq a, Show a) => String -> Expression a -> Expression a -> Expression a
substitute name val exp0 =
  map0
    (\nm ->
       if nm == name
         then val
         else undefined)
    Const
    Fun
    (\f -> apply (substitute name val f))
    exp0
