{- Testfor Statement -}
module TestStatement where

import Statement
import Test.QuickCheck

test = do
        quickCheck checkP1
        quickCheck checkP2
        quickCheck checkP3
        quickCheck checkP4
        quickCheck checkP5
        quickCheck checkP6
        quickCheck checkP7
        quickCheck checkP8
        quickCheck checkP9
        quickCheck checkP10
        quickCheck checkP11

--The checks are maybe a little bit "hacky" by converting to string but for this lab was it convenient and good enough
checkP1 = show p1 == "Skip"
checkP2 = show p2 == "Read \"count\""
checkP3 = show p3 == "Write (Add (Var \"count\") (Num 1))"
checkP4 = show p4 == "Assignment \"count\" (Num 0)"
checkP5 = show p5 == "Block [Skip]"
checkP6 = show p6 == "Block [Assignment \"x\" (Num 0),Assignment \"x\" (Add (Var \"x\") (Num 1))]"
checkP7 = show p7 == "If (Var \"x\") Skip (Assignment \"x\" (Sub (Num 0) (Var \"x\")))"
checkP8 = show p8 == "While (Var \"n\") (Assignment \"n\" (Sub (Var \"n\") (Num 1)))"
checkP9 = show p9 == "While (Var \"n\") (Block [Assignment \"fac\" (Mul (Var \"fac\") (Var \"n\")),Assignment \"n\" (Sub (Var \"n\") (Num 1))])"
checkP10 = show p10 == "Block [Read \"x\",Assignment \"x\" (Add (Var \"x\") (Num 1)),Write (Var \"x\")]"
checkP11 = show p11 == "Block [Read \"n\",Assignment \"fac\" (Num 1),While (Var \"n\") (Block [Assignment \"fac\" (Mul (Var \"fac\") (Var \"n\")),Assignment \"n\" (Sub (Var \"n\") (Num 1))]),Write (Var \"fac\")]"


p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11 :: Statement.T
p1 = fromString "skip;"
p2 = fromString "read count;"
p3 = fromString "write count+1;"
p4 = fromString "count := 0;"
p5 = fromString "begin skip; end"
p6 = fromString "begin x:=0; x:=x+1; end"
p7 = fromString "if x then skip; else x:=0-x;"
p8 = fromString "while n do n:=n-1;"
s9 = "while n do begin fac:=fac*n; n:=n-1; end"
p9 = fromString s9
p10 = fromString  "begin read x ; x := x + 1 ; write x; end"
p11 = fromString  ("begin read n; fac:=1; " ++ s9 ++ " write fac; end")
