{-# LANGUAGE TemplateHaskell #-}

module T.TH where

import Data.Char
import Foreign.C.Types
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

std_namefun str nty =
  let n:ns = nameBase nty
  in "w_" ++ str ++ "_" ++ (map toLower ns)


printout :: Name -> ExpQ
printout nty = mkTFunc (nty,nf,tyf)
  where nf = std_namefun "printout" 
        tyf n = [t|Ptr (STLVector $(return (ConT n))) -> IO ()|]

create :: Name -> ExpQ
create nty = mkTFunc (nty,nf,tyf)
  where nf = std_namefun "create"
        tyf n = [t| IO (Ptr (STLVector $(return (ConT n)))) |]

push_back :: Name -> ExpQ
push_back nty = mkTFunc (nty,nf,tyf)
  where nf = std_namefun "push_back"
        tyf n = let ty = return (ConT n) in [t| Ptr (STLVector $(ty)) -> $(ty) -> IO () |]

at :: Name -> ExpQ
at nty = mkTFunc (nty,std_namefun "at",tyf)
  where tyf n = let ty = return (ConT n)
                in [t| Ptr (STLVector $(ty)) -> CInt -> IO (Ptr $(ty)) |] 

delete :: Name -> ExpQ
delete nty = mkTFunc (nty,nf,tyf)
  where nf = std_namefun "delete"
        tyf n = [t|Ptr (STLVector $(return (ConT n)))->IO ()|]
