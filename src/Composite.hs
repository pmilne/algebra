{-# LANGUAGE MultiParamTypeClasses #-}

module Composite where

class Composite t s where
    promote      :: s -> t




