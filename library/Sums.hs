{-# LANGUAGE TemplateHaskell #-}

-- | Anonymous sum types.  Like tuples, but for sum types rather than
-- product types.  Easier than using nested 'Either'.
module Sums where

import Sums.Internal

$(return . map sumDeclaration $ [0..15])
