import Data.Char
import System.IO

-- maps labels line numbers and variables to values - uses float for line numbers for simplicity
type SymTable = [(String,Float)]

data Expr = 
     Constant Float |
     Var String |
     Plus Expr Expr |
     Minus Expr Expr | 
     Mult Expr Expr | 
     Divide Expr Expr |
     LessThan Expr Expr |
     GreaterThan Expr Expr |
     LessThanEQ Expr Expr |
     GreaterThanEQ Expr Expr |
     Equals Expr Expr |
     NotEquals Expr Expr deriving (Show) 

data Stmt =
     Let String Expr |
     Print [Expr] |
     IfGoto Expr String |
     Input String deriving (Show) 

-- dummy predicate that is supposed to check if a string is a label which is a string ending with ":"
isLabel :: String -> Bool
isLabel (str:":") = True -- if str ends with a :, then it is a label and return true
isLabel _ = False

-- takes a list of tokens as strings and returns the parsed expression
parseExpr :: [String] -> Expr
parseExpr (e1:"+":e2:[]) = Plus (parseExpr [e1]) (parseExpr [e2]) -- parses addition expression ie: ["x", "+", "1"] = Plus x 1
parseExpr (e1:"-":e2:[]) = Minus (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"*":e2:[]) = Mult (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"/":e2:[]) = Divide (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"<":e2:[]) = LessThan (parseExpr [e1]) (parseExpr[e2])
parseExpr (e1:">":e2:[]) = GreaterThan (parseExpr [e1]) (parseExpr[e2])
parseExpr (e1:"<=":e2:[]) = LessThanEQ (parseExpr [e1]) (parseExpr[e2])
parseExpr (e1:">=":e2:[]) = GreaterThanEQ (parseExpr [e1]) (parseExpr[e2])
parseExpr (e1:"==":e2:[]) = Equals (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"!=":e2:[]) = NotEquals (parseExpr [e1]) (parseExpr [e2])
parseExpr [x] = if (isAlpha (head x)) then (Var x) else (Constant (read x)) -- pattern matches the most basic elements, ie: var names and constants (turns constant into int)

-- takes the first token which should be a keyword and a list of the remaining tokens and returns the parsed Stmt
parseStmt :: String -> [String] -> Stmt
parseStmt "let" (v:"=":expr) = Let v (parseExpr expr)
parseStmt "print" expr = Print [parseExpr expr]
parseStmt "if" rest = IfGoto (parseExpr (init (init rest))) (last rest)
parseStmt "input" [varName] = Input varName

-- takes a list of tokens and returns the parsed statement - the statement may include a leading label
parseLine :: [String] -> SymTable -> Float -> Stmt
parseLine (first:rest) env lineNum =
      if (isLabel first) then (((first,lineNum):env) >> (parseLine rest)) -- if the first string is a label, then run parseLine again on the remaining strings, which now should be a Statement
      else parseStmt first rest -- if the first string isnt a label, then this line is a Statement

-- takes a variable name and a ST and returns the value of that variable or zero if the variable is not in the ST
lookupVar :: String -> SymTable -> Float
lookupVar name [] = 0
lookupVar name ((id,v):rest) = if (id == name) then v else lookupVar name rest

-- evaluates the given Expr with the variable values found in the given ST
eval :: Expr -> SymTable -> Float
eval (Var v) env = lookupVar v env
eval (Constant v) _ = v
eval (Plus e1 e2) env = (eval e1 env) + (eval e2 env)
eval (Minus e1 e2) env = (eval e1 env) - (eval e2 env)
eval (Mult e1 e2) env = (eval e1 env) * (eval e2 env)
eval (Divide e1 e2) env = (eval e1 env) / (eval e2 env)
eval (LessThan e1 e2) env = if (eval e1 env) < (eval e2 env) then 1 else 0
eval (GreaterThan e1 e2) env = if (eval e1 env) > (eval e2 env) then 1 else 0
eval (LessThanEQ e1 e2) env = if (eval e1 env) <= (eval e2 env) then 1 else 0
eval (GreaterThanEQ e1 e2) env = if (eval e1 env) >= (eval e2 env) then 1 else 0
eval (Equals e1 e2) env = if (eval e1 env) == (eval e2 env) then 1 else 0
eval (NotEquals e1 e2) env = if (eval e1 env) /= (eval e2 env) then 1 else 0

-- given a statement, a ST, line number, input and previous output, return an updated ST, input, output, and line number
-- this starter version ignores the input and line number
-- Stmt, SymTable, progCounter, input, output, (SymTable', input', output', progCounter)
perform:: Stmt -> SymTable -> Float -> [String] ->String -> (SymTable, [String], String, Float)
perform (Print e) env lineNum input output = (env, input, output++(show (eval (head e) env)++"\n"), lineNum+1) -- Print e now takes e as [String], so find a way to make it print the list
perform (Let id e) env lineNum input output = ((id,(eval e env)):env, input, output, lineNum+1)
perform (IfGoto expr label) env lineNum input output = if (eval expr) == 1 then (env, input, output, (lookupVar label env))
perform (Input varName) env lineNum input output = ((varName, (read (getLine))):env, )

-- given a list of Stmts, a ST, and current output, perform all of the statements in the list and return the updated output String
run :: [Stmt] -> SymTable -> String -> Float -> String
run [] _ output _ = output
run (curr:rest) env output lineNum= 
    let (env1, _, output1, line) = perform curr env lineNum [] output in run rest env1 output1 line

-- given list of list of tokens, a ST, return the list of parsed Stmts and ST storing mapping of labels to line numbers
parseTest :: [[String]] -> SymTable -> ([Stmt], SymTable)
parseTest []  st = ([], st)
parseTest []
-- needs completing for partial credit

main = do
     pfile <- openFile "nano1.txt" ReadMode
     contents <- hGetContents pfile
     let in putStr (run )
     putStr (run (map parseLine ((map words (lines contents)) ST lineNum) [] "" 1) -- calls parseLine on each line in contents, which eventually returns a list of Statements
     hClose pfile
