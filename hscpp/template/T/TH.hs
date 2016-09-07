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

std_namefun str nty = "w_" ++ str ++ "_" ++ (map toLower (nameBase nty))


printout :: Name -> ExpQ
printout nty = mkTFunc (nty,nf,tyf)
  where nf = std_namefun "printout" 
        tyf n = do
          io <- [t|IO ()|]
          let arg = ConT (mkName "Ptr") `AppT` (ConT (mkName "STLVector") `AppT` ConT n)
          return (ArrowT `AppT` arg `AppT` io)

create :: Name -> ExpQ
create nty = mkTFunc (nty,nf,tyf)
  where nf = std_namefun "create"
        tyf n = do
          return (ConT (mkName "IO") `AppT` (ConT (mkName "Ptr") `AppT` ((ConT (mkName "STLVector") `AppT` ConT n))))

push_back :: Name -> ExpQ
push_back nty = mkTFunc (nty,nf,tyf)
  where nf = std_namefun "push_back"
        tyf n = do
          io <- [t|IO ()|]
          let arg1 = ConT (mkName "Ptr") `AppT` (ConT (mkName "STLVector") `AppT` ConT n)
              arg2 = ConT n
          return (ArrowT `AppT` arg1 `AppT` (ArrowT `AppT` arg2 `AppT` io))



