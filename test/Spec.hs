import TestParser
import TestExpr

main :: IO ()
main = do
        putStrLn "--- Testing parser ---"
        TestParser.test
        putStrLn "--- Done ---"
        putStrLn "--- Testing EXPR ---"
        TestExpr.test
        putStrLn "--- Done ---"
