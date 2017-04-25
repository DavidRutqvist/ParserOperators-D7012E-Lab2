{- Test for Program -}
module TestProgram where

import Program
import Test.QuickCheck

test = do
  quickCheck checkRp
  quickCheck checkRp1
  quickCheck checkRp2
  quickCheck checkRp3
  quickCheck checkRp2b
  quickCheck checkRp3b

checkRp = rp == [3, 6, 9, 12, 15]
checkRp1 = rp1 == [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 10000000000]
checkRp2 = rp2 == [5, 4, 3, 2, 1]
checkRp3 = rp3 == [5, 4, 3, 2, 1, 0]
checkRp2b = rp2b == []
checkRp3b = rp3b == [0]

p, p1, p2, p3 :: Program.T
p = fromString  ("\
\read k;\
\read n;\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      skip;\
\    else\
\      write m;\
\    m := m + 1;\
\  end")

p1 = fromString  ("\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\
\    p := p*10;\
\    n :=q;\
\  end\
\write s;")

p2 = fromString ("\
\read n;\
\while n do\
\  begin\
\    write n;\
\    n := n - 1;\
\  end\
\")

p3 = fromString ("\
\read n;\
\repeat\
\  begin\
\    write n;\
\    n := n - 1;\
\  end\
\until 0-n;\
\")

sp = putStr (toString p)

rp = Program.exec p [3,16]

rp1 = Program.exec p1 [1024, 2]

rp2 = Program.exec p2 [5]

rp3 = Program.exec p3 [5]

rp2b = Program.exec p2 [0]

rp3b = Program.exec p3 [0]
