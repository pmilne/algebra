{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Numeral4 where

import Domains.Ring

type Numeral4 = forall a. ((a -> a) -> (a -> a))

--instance Ring Numeral4 where