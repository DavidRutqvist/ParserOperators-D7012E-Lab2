{- Test for Expr-}
module TestExpr where

import qualified Dictionary
import Expr
import Test.QuickCheck

test = do
        quickCheck n1
        quickCheck n2
        quickCheck n3
        quickCheck n4
        quickCheck $ expectFailure $ seq (n21) True
        quickCheck $ expectFailure $ seq (n31) True


dict = Dictionary.insert ("x", 1) $
       Dictionary.insert ("y", 2) $
       Dictionary.empty


testValue string = value (fromString string) dict

n1 = (testValue "1") == 1
n2 = (testValue "x") == 1
n3 = (testValue "x+y") == 3
n4 = (testValue "x-y-y") == -3
n21 = testValue "1/(2-y)" == 1 {-  Expr.value: division by 0 -}
n31 = testValue "2+z" == 2   {-  Expr.value: undefined variable z -}
