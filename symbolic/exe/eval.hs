{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

--
import           Test.Delta
import           Test.Fib
import           Test.Rule
import           Test.Simple


main :: IO ()
main = do
  -- delta_nosimplify
  -- putStr "\n\n\n\n\n"
  -- delta_simplify
  -- delta_eval
  -- test_differentiation
  delta_rule
    
