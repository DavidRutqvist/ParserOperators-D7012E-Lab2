import TestParser
import TestExpr
import TestStatement

main :: IO ()
main = do
        putStrLn "--- Testing parser ---"
        TestParser.test
        putStrLn "--- Done ---"
        putStrLn "--- Testing EXPR ---"
        TestExpr.test
        putStrLn "--- Done ---"
        putStrLn "--- Testing Statement ---"
        TestStatement.test
        putStrLn "--- Done ---"
