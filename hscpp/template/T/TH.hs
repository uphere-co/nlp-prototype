{-# LANGUAGE TemplateHaskell #-}

module T.TH where

import Data.Char
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import T

mkTFunc :: (Name,Name->String,Name-> Q Type) -> ExpQ  
mkTFunc (nty,nf,tyf) = do
  let fn = nf nty
  n <- newName fn
  d <- forImpD CCall unsafe fn n (tyf nty)
  addTopDecls [d]
  [|$(varE n)|]

printout :: Name -> ExpQ
printout nty = mkTFunc (nty,nf,tyf)
  where nf nty =
          case nameBase nty of
            "Int"    -> "_Z8printoutIiEvPSt6vectorIT_SaIS1_EE"
            "Double" -> "_Z8printoutIdEvPSt6vectorIT_SaIS1_EE"

        tyf n = do
          io <- [t|IO ()|]
          let arg = ConT (mkName "Ptr") `AppT` (ConT (mkName "STLVector") `AppT` ConT n)
          return (ArrowT `AppT` arg `AppT` io)

create :: Name -> ExpQ
create nty = mkTFunc (nty,nf,tyf)
  where nf nty =
          case nameBase nty of
            "Int"    -> "_Z6createIiEPSt6vectorIT_SaIS1_EEv"
            "Double" -> "_Z6createIdEPSt6vectorIT_SaIS1_EEv"
        tyf n = do
          return (ConT (mkName "IO") `AppT` (ConT (mkName "Ptr") `AppT` ((ConT (mkName "STLVector") `AppT` ConT n))))

push_back :: Name -> ExpQ
push_back nty = mkTFunc (nty,nf,tyf)
  where nf nty =
          case nameBase nty of
            "Int"    -> "_Z9push_backIiEvPSt6vectorIT_SaIS1_EES1_"
            "Double" -> "_Z9push_backIdEvPSt6vectorIT_SaIS1_EES1_"
        tyf n = do
          io <- [t|IO ()|]
          let arg1 = ConT (mkName "Ptr") `AppT` (ConT (mkName "STLVector") `AppT` ConT n)
              arg2 = ConT n
          return (ArrowT `AppT` arg1 `AppT` (ArrowT `AppT` arg2 `AppT` io))



