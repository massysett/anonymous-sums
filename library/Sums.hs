{-# LANGUAGE TemplateHaskell #-}

module Sums where

import Sums.Internal

$(return . map sumDeclaration $ [0..15])
