module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program ([Statement.T])
instance Parse T where
  parse = parseProgram
  toString = unparseProgram

exec (Program stmts) input = Statement.exec stmts Dictionary.empty input

parseProgram = iter Statement.parse >-> buildProgram
buildProgram (stmts) = Program stmts

unparseProgram (Program stmts) = unparse stmts

unparse [] = ""
unparse (stmt: stmts) = Statement.toString stmt ++ unparse stmts
