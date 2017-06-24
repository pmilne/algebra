module Hack where

import Prelude hiding (Rational)

import Field
import Exponentiative
import Trigonometric
import Applicable

import Rational

instance Exponentiative (Rational a) where
  _ ^ _   = undefined
  log _ _ = undefined
  ln _    = undefined
  exp _   = undefined
  sqrt    = undefined
  two     = undefined

instance Trigonometric (Rational a) where
  sin _ = undefined
  cos _ = undefined
  tan _ = undefined
  asin _ = undefined
  acos _ = undefined
  atan _ = undefined

instance Applicable (Rational a) where
  apply _ _ = undefined

