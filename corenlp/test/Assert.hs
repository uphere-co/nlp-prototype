module Assert where
    
assert :: Bool -> IO ()
assert True = return ()
assert False = error "Assertion error"
