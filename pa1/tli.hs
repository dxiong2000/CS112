-- Daniel Xiong dxiong5@ucsc.edu id#1660652
-- tli.hs
-- due 10/21/19
-- Pair Programming partner: Scott Zin nzin@ucsc.edu id#1679510


import Data.Char
import System.IO
import System.Environment
import System.Exit
import Data.Typeable

-- maps labels line numbers and variables to values - uses float for line numbers for simplicity
type SymTable = [(String,Float)]

data Expr = 
     Constant Float |
     Var String |
     Plus Expr Expr |
     Minus Expr Expr | 
     Times Expr Expr | 
     Div Expr Expr |
     LT_ Expr Expr |
     GT_ Expr Expr |
     LE_ Expr Expr |
     GE_ Expr Expr |
     EQ_ Expr Expr |
     NEQ_ Expr Expr |
     ExprError String deriving (Show, Eq) 

data Stmt =
     Let String Expr |
     Print [Expr] |
     If Expr String |
     Input String deriving (Show) 

-- dummy predicate that is supposed to check if a string is a label which is a string ending with ":"
isLabel :: String -> Bool
isLabel str = if ((last str) == ':') then True else False

-- takes a list of tokens as strings and returns the parsed expression
parseExpr :: [String] -> Expr
parseExpr [x] = if (isAlpha (head x)) then (Var x) else if (isPunctuation (head x)) then (ExprError x) else (Constant (read x))-- pattern matches the most basic elements, ie: var names and constants (turns constant into int)
parseExpr (e1:"+":e2:[]) = Plus (parseExpr [e1]) (parseExpr [e2]) -- parses addition expression ie: ["x", "+", "1"] = Plus x 1
parseExpr (e1:"-":e2:[]) = Minus (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"*":e2:[]) = Times (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"/":e2:[]) = Div (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"<":e2:[]) = LT_ (parseExpr [e1]) (parseExpr[e2])
parseExpr (e1:">":e2:[]) = GT_ (parseExpr [e1]) (parseExpr[e2])
parseExpr (e1:"<=":e2:[]) = LE_ (parseExpr [e1]) (parseExpr[e2])
parseExpr (e1:">=":e2:[]) = GE_ (parseExpr [e1]) (parseExpr[e2])
parseExpr (e1:"==":e2:[]) = EQ_ (parseExpr [e1]) (parseExpr [e2])
parseExpr (e1:"!=":e2:[]) = NEQ_ (parseExpr [e1]) (parseExpr [e2])


-- helper function for parseStmt
-- takes in unprocessed [String] and returns [Expr]
parseStmtPrintHelper :: [String] -> [String] -> [Expr] -> [Expr]
parseStmtPrintHelper [x] curString exprList = if ((last x) /= '"') then exprList++[(parseExpr (curString++[x]))] else exprList++[(parseExpr [(unwords (curString++[x]))])]
parseStmtPrintHelper (x:xs) curString exprList = 
    if (x == ",")
    then if ((last (last curString)) == '"') then parseStmtPrintHelper xs [] (exprList++[(parseExpr [(unwords curString)])]) else parseStmtPrintHelper xs [] (exprList++[(parseExpr curString)])
    else parseStmtPrintHelper xs (curString++[x]) exprList

-- takes the first token which should be a keyword and a list of the remaining tokens and returns the parsed Stmt
parseStmt :: String -> [String] -> Float -> Stmt
parseStmt "let" (v:"=":expr) _ = Let v (parseExpr expr)
parseStmt "print" expr _ = Print (parseStmtPrintHelper expr [] [])
parseStmt "if" rest _ = If (parseExpr (init (init rest))) (last rest)
parseStmt "input" [varName] _ = Input varName
parseStmt _ _ lineNum = error ("Syntax error on line "++(show lineNum))


-- recursive function that calls parseLine on each iteration and builds a list of Statements
runParseLine :: [[String]] -> [Stmt] -> SymTable -> Float -> ([Stmt], SymTable)
runParseLine [] stmtList env _ = (stmtList, env)
runParseLine (head:rest) stmtList env lineNumber = 
    let (statement, env1) = parseLine head env lineNumber in runParseLine rest (stmtList++[statement]) env1 (lineNumber+1)

-- takes a list of tokens and returns the parsed statement - the statement may include a leading label
parseLine :: [String] -> SymTable -> Float -> (Stmt, SymTable)
parseLine (first:rest) env lineNum =
     if (isLabel first) 
     then parseLine rest ((first,lineNum):env) lineNum -- if the first string is a label, then run parseLine again on the remaining strings, which now should be a Statement
     else ((parseStmt first rest lineNum), env) -- if the first string isnt a label, then this line is a Statement

-- takes a variable name and a ST and returns the value of that variable or zero if the variable is not in the ST
lookupVar :: String -> SymTable -> Float -> Bool -> Float
lookupVar name [] lineNum isLab = if isLab then error ("Illegal goto "++(show (init name))++" at line "++(show lineNum)) else error ("Undefined variable name "++(show name)++" at line "++(show lineNum))
lookupVar name ((id,v):rest) lineNum isLab = if (id == name) then v else lookupVar name rest lineNum isLab


-- evaluates the given Expr with the variable values found in the given ST
eval :: Expr -> SymTable -> Float -> Float
eval (Var v) env lineNum = lookupVar v env lineNum False
eval (Constant v) _ _ = v
eval (Plus e1 e2) env lineNum = (eval e1 env lineNum) + (eval e2 env lineNum)
eval (Minus e1 e2) env lineNum = (eval e1 env lineNum) - (eval e2 env lineNum)
eval (Times e1 e2) env lineNum = (eval e1 env lineNum) * (eval e2 env lineNum)
eval (Div e1 e2) env lineNum = (eval e1 env lineNum) / (eval e2 env lineNum)
eval (LT_ e1 e2) env lineNum = if (eval e1 env lineNum) < (eval e2 env lineNum) then 1 else 0
eval (GT_ e1 e2) env lineNum = if (eval e1 env lineNum) > (eval e2 env lineNum) then 1 else 0
eval (LE_ e1 e2) env lineNum = if (eval e1 env lineNum) <= (eval e2 env lineNum) then 1 else 0
eval (GE_ e1 e2) env lineNum = if (eval e1 env lineNum) >= (eval e2 env lineNum) then 1 else 0
eval (EQ_ e1 e2) env lineNum = if (eval e1 env lineNum) == (eval e2 env lineNum) then 1 else 0
eval (NEQ_ e1 e2) env lineNum = if (eval e1 env lineNum) /= (eval e2 env lineNum) then 1 else 0

-- helper function for perform's Print pattern match
-- takes list of expressions [Expr] and adds them to output
printHelper :: [Expr] -> SymTable -> String -> Float -> String
printHelper [] _ output lineNum = output++"\n"
printHelper ((ExprError e):es) env output lineNum = 
    let out = unwords.words $ reverse . dropWhile (not.isAlpha) . reverse $ dropWhile (not.isAlpha) (show e) 
    in printHelper es env (output++out++" ") lineNum
printHelper (e:es) env output lineNum = let out = show (eval e env lineNum) in printHelper es env (output++out++" ") lineNum

-- given a statement, a ST, line number, input and previous output, return an updated ST, input, output, and line number
-- this starter version ignores the input and line number
-- Stmt, SymTable, progCounter, input, output, (SymTable', input', output', progCounter)
-- Stmt : Print [(x + 1), (x + 2)]
perform :: Stmt -> SymTable -> Float -> [String] -> String -> (SymTable, [String], String, Float)
perform (Print exprList) env lineNum input output = (env, input, (printHelper exprList env output lineNum), lineNum+1) -- Print e now takes e as [String], so find a way to make it print the list
perform (Let id e) env lineNum input output = ((id,(eval e env lineNum)):env, input, output, lineNum+1)
perform (If expr label) env lineNum input output = if ((eval expr env lineNum) == 1) then (env, input, output, (lookupVar (label++":") env lineNum True)) else (env, input, output, lineNum+1)
perform (Input varName) env lineNum [] output = error ("Prelude.read: no parse")
perform (Input varName) env lineNum (x:xs) output = ((varName,(read x)):env, xs, output, lineNum+1)

-- helper function for run 
-- returns Stmt at its respective line number
getStmtAtLineNum :: [Stmt] -> Float -> Stmt
getStmtAtLineNum (x:_) 1 = x
getStmtAtLineNum (x:xs) i = getStmtAtLineNum xs (i-1)

-- given a list of Stmts, a ST, and current output, perform all of the statements in the list and return the updated output String
run :: [Stmt] -> SymTable -> [String] -> String -> Float -> String
run stmtList env input output lineNum = 
    if (lineNum == ((fromIntegral (length stmtList)) + 1.0))
    then output
    else let (env1, input1, output1, newLineNum) = perform (getStmtAtLineNum stmtList lineNum) env lineNum input output in run stmtList env1 input1 output1 newLineNum

-- given list of list of tokens, a ST, return the list of parsed Stmts and ST storing mapping of labels to line numbers
parseTest :: [[String]] -> SymTable -> ([Stmt], SymTable)
parseTest []  st = ([], st)
parseTest allLines env = runParseLine allLines [] env 1


main = do
     input <- getContents -- reads user input from stdin
     args <- getArgs -- gets command line args
     pfile <- openFile (head args) ReadMode -- opens file
     contents <- hGetContents pfile -- gets file contents
     let input1 = words input -- formats user input into list
     let allLines = (map words (lines contents)) -- formats file contents into a [[String]]
     let (stmtList, env) = runParseLine allLines [] [] 1 -- gets list of statements and symtable currently storing only labels
     let output = run stmtList env input1 "" 1 -- gets output from run
     putStr output
     hClose pfile
