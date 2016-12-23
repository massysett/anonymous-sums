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
    ty = [t| Lens.Prism $(big) $(big') $(little) $(little') |]
    var = T.varT . T.mkName
    little = var ("t" ++ show thisCtor)
    little' = var ("t" ++ show thisCtor ++ "'")
    mkTypes maker = foldl' T.appT start tys
      where
        start = T.conT (T.mkName ("S" ++ show nCtors))
        tys = map maker [1..nCtors]
    big = mkTypes (\n -> var ("t" ++ show n))
    big' = mkTypes f
      where
        f n
          | n /= thisCtor = var ("t" ++ show n)
          | otherwise = var ("t" ++ show n ++ "'")


-- | Given the number of constructors and the particular constructor
-- number, return the prism itself.
prismDecl :: Int -> Int -> T.DecQ
prismDecl nCtors thisCtor = T.valD prismPat prismBody []
  where
    otherCtorName n = T.mkName ("S" ++ show nCtors ++ "_" ++ show n)
    thisCtorName = otherCtorName thisCtor
    prismPat = T.varP (T.mkName ("_S" ++ show nCtors ++ "_" ++ show thisCtor))
    prismBody = T.normalB expn
      where
        expn = [| Lens.prism $(make) $(decon) |]
          where
            make = T.conE thisCtorName
            decon = do
              x <- T.newName "x"
              let caseExpn = T.caseE (T.varE x) matches
              T.lam1E (T.varP x) caseExpn
    matches = found : notFounds
    found = do
      x <- T.newName "x"
      let pat = T.conP thisCtorName [T.varP x]
          body = T.normalB [| Right $(T.varE x) |]
      T.match pat body []
    notFounds = map mkNotFound . filter (/= thisCtor) $ [1..nCtors]
    mkNotFound i = do
      x <- T.newName "x"
      let pat = T.conP (otherCtorName i) [T.varP x]
          body = T.normalB
            [| Left $ $(T.conE (otherCtorName i)) $(T.varE x) |]
      T.match pat body []

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
