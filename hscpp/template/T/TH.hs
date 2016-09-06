{-# LANGUAGE TemplateHaskell #-}

module T.TH where

import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import T

push_back_int :: ExpQ
push_back_int = do
  let fname = "push_back"
  n <- newName fname
  d <- forImpD CCall unsafe fname n [t|Ptr (STLVector Int) -> IO ()|]
  addTopDecls [d]
  [|$(varE n)|]

-- push_back 


