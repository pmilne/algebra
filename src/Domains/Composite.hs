{-# LANGUAGE MultiParamTypeClasses #-}

module Domains.Composite where

class Composite t s where
    promote      :: s -> t




