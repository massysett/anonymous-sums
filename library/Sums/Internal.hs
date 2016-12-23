{-# LANGUAGE TemplateHaskell #-}
module Sums.Internal where

import Data.Foldable (foldl')
import qualified Language.Haskell.TH as T
import qualified Control.Lens as Lens

-- | Given the number of constructors, create a new data type.
-- Derives Eq, Ord, and Show, except for S0, which has no derives.
sumDeclaration :: Int -> T.Dec
sumDeclaration i = T.DataD cxt name types Nothing ctors derives
  where
    cxt = []
    name = T.mkName ("S" ++ show i)
    types = map mkType [1..i]
      where
        mkType n = T.PlainTV (T.mkName ("t" ++ show n))
    ctors = map mkCtor [1..i]
      where
        mkCtor n = T.NormalC (T.mkName ("S" ++ show i ++ "_" ++ show n))
          [ ( T.Bang T.NoSourceUnpackedness T.NoSourceStrictness
            , T.VarT (T.mkName ("t" ++ show n))
            ) ]
    derives
      | i == 0 = []
      | otherwise = map T.ConT [''Eq, ''Ord, ''Show]

-- | Given the number of constructors and this particular constructor
-- number, return a type signature for a prism.
prismSig :: Int -> Int -> T.DecQ
prismSig nCtors thisCtor = T.sigD name ty
  where
    name = T.mkName ("_S" ++ show nCtors ++ "_" ++ show thisCtor)
    ty = [t| Lens.Prism' $(sumTy) $(fieldTy) |]
    sumTy = foldl' T.appT start tys
      where
        start = T.conT (T.mkName ("S" ++ show nCtors))
        tys = map mkTy [1..nCtors]
          where
            mkTy n = T.varT (T.mkName ("t" ++ show n))
    fieldTy = T.varT (T.mkName ("t" ++ show thisCtor))

-- | Given the number of constructors and the particular constructor
-- number, return the prism itself.
prismDecl :: Int -> Int -> T.DecQ
prismDecl nCtors thisCtor = T.valD prismPat prismBody []
  where
    prismPat = T.varP (T.mkName ("_S" ++ show nCtors ++ "_" ++ show thisCtor))
    prismBody = T.normalB expn
      where
        expn = [| Lens.prism' $(make) $(decon) |]
          where
            make = T.conE thisCtorName
            decon = do
              x <- T.newName "x"
              let caseExpn = T.caseE (T.varE x) matches
              T.lam1E (T.varP x) caseExpn
    matches
      | nCtors == 1 = [found]
      | otherwise = [found, notFound]
    thisCtorName = T.mkName ("S" ++ show nCtors ++ "_" ++ show thisCtor)
    found = do
      x <- T.newName "x"
      let pat = T.conP thisCtorName [T.varP x]
          body = T.normalB [| Just $(T.varE x) |]
      T.match pat body []
    notFound = T.match T.wildP (T.normalB [| Nothing |]) []

-- | Given the number of ctors and this ctor number, return a
-- signature and the prism itself.
prismSigAndDecl :: Int -> Int -> T.DecsQ
prismSigAndDecl nCtors thisCtor = sequence
  [ prismSig nCtors thisCtor
  , prismDecl nCtors thisCtor
  ]

-- | Given the number of ctors, return all prisms.
prismsForSingleType :: Int -> T.DecsQ
prismsForSingleType i
  = fmap concat . traverse (prismSigAndDecl i) $ [1..i]
