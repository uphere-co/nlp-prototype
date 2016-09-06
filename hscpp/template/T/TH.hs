{-# LANGUAGE TemplateHaskell #-}

module T.TH where

import Data.Char
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import T

mkTFunc :: (Name,String,Name-> Q Type) -> ExpQ  
mkTFunc (nty,fn,tyf) = do
  n <- newName fn
  d <- forImpD CCall unsafe fn n (tyf nty)
  addTopDecls [d]
  [|$(varE n)|]

testfunction :: Name -> ExpQ
testfunction nty = mkTFunc (nty,"_Z8printoutIiEvPSt6vectorIT_SaIS1_EE",tyf)
  where tyf n = do
          io <- [t|IO ()|]
          let arg = ConT (mkName "Ptr") `AppT` (ConT (mkName "STLVector") `AppT` ConT n)
          return (ArrowT `AppT` arg `AppT` io)

create :: Name -> ExpQ
create nty = mkTFunc (nty,"_Z6createIiEPSt6vectorIT_SaIS1_EEv",tyf)
  where tyf n = do
          return (ConT (mkName "IO") `AppT` (ConT (mkName "Ptr") `AppT` ((ConT (mkName "STLVector") `AppT` ConT n))))

push_back :: Name -> ExpQ
push_back nty = mkTFunc (nty,"_Z9push_backIiEvPSt6vectorIT_SaIS1_EES1_",tyf)
  where tyf n = do
          io <- [t|IO ()|]
          let arg1 = ConT (mkName "Ptr") `AppT` (ConT (mkName "STLVector") `AppT` ConT n)
              arg2 = ConT n
          return (ArrowT `AppT` arg1 `AppT` (ArrowT `AppT` arg2 `AppT` io))



