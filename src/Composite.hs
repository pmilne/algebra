module Composite where

import Complex

class Composite t where
    promote      :: a -> t a

instance Composite Complex where
    promote n = n :+ n




