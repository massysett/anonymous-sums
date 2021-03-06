{-# LANGUAGE TemplateHaskell #-}

-- | Prisms to extract and manipulate values from a sum type.
module Sums.Prisms where

import Sums
import Sums.Internal

$( fmap (reverse . concat) . traverse prismsForSingleType $ [1..15] )
