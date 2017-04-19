{- Test for Parser.hs -}
module TestParser where

import Prelude hiding (fail, return)
import Parser
import Test.QuickCheck

test = do
        quickCheck l1
        quickCheck l2
        quickCheck l3
        quickCheck w1
        quickCheck w2
        quickCheck c1
        quickCheck c2
        quickCheck c3
        quickCheck r1
        quickCheck $ expectFailure $ seq (r2) True
        quickCheck a4

l1 = letter "abc" == Just('a',"bc")

l2 = letter "123" == Nothing
l3 = letter "" == Nothing

w1 = spaces "abc" == Just("","abc")
w2 = spaces "  \t abc" == Just("  \t ","abc")

c1 = chars 2 "abc" == Just ("ab","c")
c2 = chars 0 "ab" == Just ("","ab")
c3 = chars 3 "ab" == Nothing

r1 = require ":=" ":= 1" == Just (":=","1")
r2 = require "else" "then" == Just ("else", "then")

a4 = (accept "read" -# word) "read count" == Just ("count","")
