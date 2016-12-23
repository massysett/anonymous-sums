{-# LANGUAGE TemplateHaskell #-}
module Sums.Prisms where

import Sums
import Sums.Internal

$( fmap (reverse . concat) . traverse prismsForSingleType $ [1..15] )
