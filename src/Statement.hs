module Statement(T, parse, toString, fromString, exec) where
import qualified Dictionary
import qualified Expr
import           Parser     hiding (T)
import           Prelude    hiding (fail, return)
type T = Statement
data Statement =
        Assignment String Expr.T
      | If Expr.T Statement Statement
      | Skip
      | While Expr.T Statement
      | Read String
      | Write Expr.T
      | Block [Statement]--Begin {Statements} End
      | Repeat Statement Expr.T
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e

if' = accept "if" -# Expr.parse #- require "then" # parseSingle #- require "else" # parseSingle >-> buildIf
buildIf ((expression, thenStatements), elseStatements) = If expression thenStatements elseStatements

skip = accept "skip" #- require ";" >-> buildSkip
buildSkip (_) = Skip

while = accept "while" -# Expr.parse #- require "do" # parseSingle >-> buildWhile
buildWhile (e, s) = While e s

read' = accept "read" -# word #- require ";" >-> buildRead
buildRead (v) = Read v

write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite (e) = Write e

block = accept "begin" -# parseMany #- require "end" >-> buildBlock
buildBlock (statements) = Block statements

doWhile = accept "repeat" -# parseSingle #- require "until" # Expr.parse #- require ";" >-> buildDoWhile
buildDoWhile (s, e) = Repeat s e

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (If cond thenStmts elseStmts: stmts) dict input =
    if (Expr.value cond dict)>0
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input
exec (Skip: stmts) dict input = exec stmts dict input
exec (Assignment var expression: stmts) dict input = exec stmts (Dictionary.insert (var, Expr.value expression dict) dict) input
exec (While cond statement: stmts) dict input =
  if (Expr.value cond dict) > 0
  then exec (statement : (While cond statement) : stmts) dict input
  else exec stmts dict input
exec (Read var: stmts) dict (inputVar: remainingInput) = exec stmts (Dictionary.insert (var, inputVar) dict) remainingInput
exec (Write expression: stmts) dict input = Expr.value expression dict: exec stmts dict input
exec (Block blockStmts: stmts) dict input = exec (blockStmts ++ stmts) dict input
exec (Repeat statement cond: stmts) dict input = exec (statement: ((If cond Skip (Repeat statement cond)): stmts)) dict input
exec [] dict input = [];

unparse :: T -> [String]
unparse (If cond thenStmts elseStmts) = (("if " ++ Expr.toString cond ++ " then") : indent(unparse thenStmts)) ++
  ("else" : indent(unparse elseStmts))
unparse (Skip) = "skip;" : []
unparse (Assignment var expression) = (var ++ " := " ++ Expr.toString expression ++ ";") : []
unparse (While cond statement) = ("while " ++ Expr.toString cond ++ " do") : indent(unparse statement)
unparse (Read var) = ("read " ++ var ++ ";") : []
unparse (Write expression) = ("write " ++ Expr.toString expression ++ ";") : []
unparse (Block blockStmts) = "begin" : indent(unparseMany blockStmts) ++ ("end" : [])
unparse (Repeat statement cond) = ("repeat" : indent(unparse statement)) ++ (("until " ++ Expr.toString cond ++ ";") : [])

unparseMany :: [T] -> [String]
unparseMany [] = []
unparseMany (stmt: stmts)= unparse stmt ++ unparseMany stmts

toText :: [String] -> String
toText [] = ""
toText (line: stringlines) = line ++ "\n" ++ toText stringlines

indent :: [String] -> [String]
indent [] = []
indent (line: stringlines) = ('\t' : line) : indent stringlines

parseSingle = (assignment ! if' ! skip ! while ! read' ! write ! block ! doWhile)
parseMany = iter parseSingle

instance Parse Statement where
  parse = parseSingle
  toString = toText . unparse
