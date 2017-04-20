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

parseSingle = (assignment ! if' ! skip ! while ! read' ! write ! block ! doWhile)
parseMany = iter parseSingle

instance Parse Statement where
  parse = parseSingle
  toString = error "Statement.toString not implemented"
