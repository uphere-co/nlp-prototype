{-# LANGUAGE TemplateHaskell #-}

module T.TH where

import Data.Char
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax


import T

testfunction :: Name -> ExpQ
testfunction ty = do
  let fname = "testfunction"
  TyConI tyCon <- reify ty
  n <- newName fname
  let fname' = fname ++ "_" ++ map toLower (nameBase ty)
  d <- forImpD CCall unsafe fname' n $ do
         io <- [t|IO ()|]
         let arg = ConT (mkName "Ptr") `AppT` (ConT (mkName "STLVector") `AppT` ConT ty)
         return (ArrowT `AppT` arg `AppT` io)


  -- [t|Ptr (STLVector tyCon) -> IO ()|]
  addTopDecls [d]
  [|$(varE n)|]

-- push_back 


