{-
From: http://5outh.blogspot.com/2013/05/symbolic-calculus-in-haskell.html
-}
module Expression where

import Prelude hiding ((+), (-), negate, (*), (/), (^), exp, log, sin , cos, tan, sqrt, asin, acos, atan)
import Field
import Exponentiative
import Trigonometric
import Applicable
--import Debug.Trace

infixl 9 ~>

data Fn a = Fn {
  name_ :: String,
  fun_ :: a -> a,
  inverse_ :: Expression a -> Expression a,
  derivative_:: Expression a -> Expression a
}

fnValue :: Expression a -> a -> a
fnValue (Fun f) = fun_ f
fnValue _ = undefined

instance Eq (Fn a) where
  Fn name1 _ _ _ == Fn name2 _ _ _= name1 == name2

instance Show (Fn a) where
  show (Fn name1 _ _ _) = name1

data Expression a = Const !a
                  | Var !String
                  | Fun !(Fn a)
                  | Lambda !(Expression a) !(Expression a)
                  | App !(Expression a) !(Expression a)
                  | Sum !(Expression a) !(Expression a)
                  | Neg !(Expression a)
                  | Prd !(Expression a) !(Expression a)
                  | Inv !(Expression a)
                  | Pow !(Expression a) !(Expression a)
                  | Log !(Expression a) !(Expression a)
                  deriving (Eq)

(~>) :: Expression a -> Expression a -> Expression a
x ~> body = Lambda x body

instance (Show a) => Show (Expression a) where
 show (Const a) = show a
 show (Var a)   = a
 show (Fun f)   = name_ f
 show (Lambda var body) = "(" ++ show var ++ " ~> " ++ show body ++ ")"
 show (App f a) = "(" ++ show f ++ " " ++ show a ++ ")"
 show (Sum a b) = "(" ++ show a ++ " + " ++ show b ++ ")"
 show (Neg a)   = "(" ++ "-" ++ show a ++ ")"
 show (Prd a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
 show (Pow a b) = "(" ++ show a ++ "^" ++ show b ++ ")"
 show (Log a b) = "(log (base " ++ show a ++ ") " ++ show b ++ ")"
 show (Inv a)   = "(" ++ "/" ++ show a ++ ")"

instance (Eq a, Additive a) => Additive (Expression a) where
  Const a + Const b = Const (a + b)
  Const a + b       = if a == zero then b else Sum (Const a) b
  a       + Const b = if b == zero then a else Sum a (Const b)
  a + b             = Sum a b
  zero              = Const zero

instance (Negatable a) => Negatable (Expression a) where
  neg (Const a) = Const (neg a)
  neg (Neg a)   = a
  neg a         = Neg a

instance (Eq a, Subtractive a) => Subtractive (Expression a) where

instance (Eq a, Additive a, Multiplicative a) => Multiplicative (Expression a) where
  Const a * Const b = Const (a * b)
  a       * Const b | b == zero = zero
                    | b == one = a
                    | otherwise = Prd (Const b) a -- if commutative we can move the constant to the front
  Const a * (Prd (Const b) c)  -- if associative, gather constants
                    = Prd (Const (a * b)) c
  Const a * b       | a == zero = zero
                    | a == one = b
                    | otherwise = Prd (Const a) b
  (Inv a) * (Inv b) = Inv (a * b)
  a * (Prd b (Inv c)) = Prd (a * b) (Inv c)
  a * (Inv b)       = if a == b then one else Prd a (Inv b)
  a       * b       = if a == b then Pow a (Const one + Const one) else Prd a b
  one               = Const one

instance (Eq a, Ring a) => Ring (Expression a) where

instance (Eq a, Field a) => Invertable (Expression a) where
  inv (Const a) = Const (inv a)
  inv a = Inv a

instance (Eq a, Field a) => Field (Expression a) where
  a / b             = a * inv b

instance (Eq a, Field a, Exponentiative a) => Exponentiative (Expression a) where
  Const a ^ Const b = Const (a ^ b)
  a       ^ Const b | b == zero = one
                    | b == one = a
                    | otherwise = Pow a (Const b)
  Const a ^ b       | a == zero = zero
                    | a == one = one
                    | otherwise = Pow (Const a) b
  a       ^ b       = Pow a b
  log a b           | b == one = zero
                    | otherwise = Log a b
  ln (Const a)      = Const (ln a)
  ln a              = App (Fun (Fn "ln" ln exp (\x -> one / x))) a
  exp (Const a)     = Const (exp a)
  exp a             = App (Fun (Fn "exp" exp ln exp)) a
  sqrt              = App (Fun (Fn "sqrt" sqrt (^ two) (\x -> one / (two * sqrt x))))
  two               = Const two


-- https://en.wikipedia.org/wiki/Differentiation_of_trigonometric_functions
instance (Eq a, Field a, Exponentiative a, Trigonometric a) => Trigonometric (Expression a) where
  sin = App (Fun (Fn "sin" sin asin cos))
  cos = App (Fun (Fn "cos" cos acos (neg . sin)))
  tan = App (Fun (Fn "tan" tan atan (\x -> one / cos x^two)))
  asin = App (Fun (Fn "asin" asin sin (\x -> one / sqrt (one - x^two))))
  acos = App (Fun (Fn "acos" acos cos (\x -> neg one / sqrt (one - x^two))))
  atan = App (Fun (Fn "atan" atan tan (\x -> one / (one + x^two))))

instance (Applicable a) => Applicable (Expression a) where
  apply {-f x-} = App {-f x-}

ev :: (Show a) => Fn a -> Expression a -> Expression a
ev fun (Const x) = Const (fun_ fun x)
ev fun e = App (Fun fun) e

map0 :: (Show b, Eq b, Field b, Exponentiative b) => (String -> b) -> (a -> b) -> (Fn a -> b) -> (Expression a -> b -> b) -> Expression a -> b
map0 mapVar mapConst mapFun mapApplyFun {-exp-} =
--  trace ("evalExpr: " ++ nm ++ " -> " ++ show val ++ " in " ++ show exp) $
  rec {-exp-} where
  rec e = case e of
                         Const a        -> mapConst a
                         Var a          -> mapVar a
                         Fun f          -> mapFun f
                         App f a        -> mapApplyFun f (rec a)
                         Neg a          -> neg (rec a)
                         Sum a b        -> rec a + rec b
                         Prd a b        -> rec a * rec b
                         Inv a          -> inv (rec a)
                         Pow a b        -> rec a ^ rec b
                         Log a b        -> log (rec a) (rec b)

eval1 :: (Show a, Eq a, Field a, Exponentiative a, Applicable a) => String -> a -> Expression a -> a
eval1 name value {-exp-} = map0 (\varName -> if name == varName then value else undefined) id undefined fnValue {-exp-}

substitute :: (Applicable a, Exponentiative a, Field a, Eq a, Show a) => (Expression a -> Expression a) -> Expression a -> Expression a
substitute val {-exp-} = map0 (\nm -> if nm == "x" then val (Var "x") else undefined) Const Fun (\f -> apply (substitute val f)) {-exp-}


derivative :: (Show a, Eq a, Field a, Exponentiative a) => Expression a -> Expression a
derivative (Lambda var body) = Lambda var (rec body) where
                             rec e = case e of
                                  (Const _)            -> zero
                                  (Var _)              -> if e == var then one else zero
                                  (App (Fun f) a)      -> rec a * derivative_ f a -- chain rule
                                  (Neg a)              -> neg (rec a)
                                  (Sum a b)            -> rec a + rec b
                                  (Prd a b)            -> a * rec b + rec a * b --product rule (ab' + a'b)
                                  (Inv a)              -> neg (rec a) / (a ^ two)
                                  --derivative (Div a b)            -> (derivative a * b - a * derivative b) / b ^ two -- quotient rule ( (a'b - b'a) / b^2 )
                                  (Pow a (Const n))    -> Const n * rec a * a ^ Const (n - one) --specialised power rule (xa^(n-1) * a')
                                  (Pow f g)            -> f ^ g * (rec f * g / f + rec g * ln f) --general power rule: https://en.wikipedia.org/wiki/Differentiation_rules#Generalized_power_rule
derivative e               = error $ "Error: dd " ++ show e

inverse :: (Show a, Eq a, Field a, Exponentiative a, Applicable a) => Expression a -> Expression a
inverse (Const _)               = undefined
inverse (Var x)                 = Var x
inverse (App (Fun f) a)         = substitute (inverse_ f) (inverse a)

inverse (Sum (Const a) b)       = substitute (\x -> neg (Const a) + x) (inverse b)
inverse (Sum a (Const b))       = substitute (\x -> x - Const b) (inverse a)
inverse (Sum _ _)               = undefined

inverse (Neg a)                 = substitute neg (inverse a)

inverse (Prd (Const a) b)       = substitute (\x -> inv (Const a) * x) (inverse b)
inverse (Prd a (Const b))       = substitute (\x -> x / Const b) (inverse a)
inverse (Prd _ _)               = undefined

inverse (Inv a)                 = substitute inv (inverse a)

inverse (Pow a (Const n))       = substitute (\x -> x ^ inv (Const n)) (inverse a)
inverse (Pow (Const a) n)       = substitute (\x -> log (Const a) x) (inverse n)
inverse (Pow _ _)               = undefined

