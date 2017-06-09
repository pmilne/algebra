module TestUtil where

assert :: Bool -> a -> a
assert False _ = error "*** assertion failed! ***"
assert _     x = x

test :: (Show a, Eq a) => String -> a -> a -> IO ()
test name expected actual = do
                                putStrLn (name ++ " = " ++ show actual)
                                assert (expected == actual) putStr "" -- oh dear

