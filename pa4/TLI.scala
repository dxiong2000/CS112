/*
 * Daniel Xiong dxiong5@ucsc.edu id#1660652
 * TLI.scala
 * Due 12/6/2019
*/

import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.StringBuilder
import scala.io.Source
import scala.io.StdIn.readLine
import util.control.Breaks._

abstract class Expr
case class Var(name: String) extends Expr
case class PrintStr(name: String) extends Expr
case class Constant(num: Double) extends Expr
case class BinOp(operator: String, op1: Expr, op2: Expr) extends Expr

abstract class Stmt
case class Let(variable: String, expr: Expr) extends Stmt
case class If(expr: Expr, label: String) extends Stmt
case class Input(variable: String) extends Stmt
case class Print(exprList: ArrayBuffer[Expr]) extends Stmt
case class No_Op() extends Stmt

object TLI {
    def eval(expr: Expr, symTable: Map[String, Double], lineNum: Int): Option[Either[String, Double]] = expr match {
        case BinOp("+",e1,e2) => Some(Right(eval(e1,symTable,lineNum).get.right.get + eval(e2,symTable,lineNum).get.right.get)) // this is ugly
        case BinOp("-",e1,e2) => Some(Right(eval(e1,symTable,lineNum).get.right.get - eval(e2,symTable,lineNum).get.right.get))
        case BinOp("*",e1,e2) => Some(Right(eval(e1,symTable,lineNum).get.right.get * eval(e2,symTable,lineNum).get.right.get))
        case BinOp("/",e1,e2) => Some(Right(eval(e1,symTable,lineNum).get.right.get / eval(e2,symTable,lineNum).get.right.get))
        case BinOp("<",e1,e2) =>
            if (eval(e1,symTable,lineNum).get.right.get < eval(e2,symTable,lineNum).get.right.get){
                return Some(Right(1))
            }
            else{
                return Some(Right(0))
            }
        case BinOp(">",e1,e2) => 
            if (eval(e1,symTable,lineNum).get.right.get > eval(e2,symTable,lineNum).get.right.get){
                return Some(Right(1))
            }
            else{
                return Some(Right(0))
            }
        case BinOp("<=", e1, e2) =>
            if (eval(e1,symTable,lineNum).get.right.get <= eval(e2,symTable,lineNum).get.right.get){
                return Some(Right(1))
            }
            else{
                return Some(Right(0))
            }
        case BinOp(">=", e1, e2) =>
            if (eval(e1,symTable,lineNum).get.right.get >= eval(e2,symTable,lineNum).get.right.get){
                return Some(Right(1))
            }
            else{
                return Some(Right(0))
            }
        case BinOp("==", e1, e2) =>
            if (eval(e1,symTable,lineNum).get.right.get == eval(e2,symTable,lineNum).get.right.get){
                return Some(Right(1))
            }
            else{
                return Some(Right(0))
            }
        case BinOp("!=", e1, e2) =>
            if (eval(e1,symTable,lineNum).get.right.get != eval(e2,symTable,lineNum).get.right.get){
                return Some(Right(1))
            }
            else{
                return Some(Right(0))
            }
        case Var(name) => Some(Right(symTable(name)))
        case Constant(num) => Some(Right(num))
        case PrintStr(name) => Some(Left(name))
	    case _ => 
            println(s"Syntax error on line $lineNum.")
            System.exit(0)
            None
    }

    def parseLine(lines: ArrayBuffer[Array[String]], stmtList: ArrayBuffer[Stmt], symTable: Map[String, Double]): (ArrayBuffer[Stmt], Map[String, Double]) = {
        
        if(lines.length == 0){
            (stmtList, symTable)
        }

        var lineNum = 0
        var statement: Stmt = null 

        for((line, i) <- lines.zipWithIndex){
            breakable {
                if(line.length == 0){
                    stmtList.append(No_Op())
                    break
                }
                lineNum = i + 1
                if(line(0).endsWith(":")){
                    symTable(line(0)) = lineNum
                    statement = parseStmt(line.drop(1), lineNum).get
                }
                else{
                    statement = parseStmt(line, lineNum).get
                }
                stmtList.append(statement)
            }
        }
        return (stmtList, symTable)
    }

    def parseStmtPrintHelper(tokens: ArrayBuffer[String], lineNum: Int): ArrayBuffer[Expr] = {
        var curExpr = new ArrayBuffer[String]()
        var exprList = new ArrayBuffer[Expr]()
        while (!tokens.isEmpty){
            var s = tokens.remove(0)

            if (tokens.isEmpty){ // if the list is now empty after removing a token
                curExpr.append(s)
                if (s.endsWith("\"")){ // if the token ends with a ", 
                    var str = curExpr.mkString(" ")
                    exprList.append(parseExpr(Array(str), lineNum).get)
                }
                else{
                    exprList.append(parseExpr(curExpr.toArray, lineNum).get)
                }
            }

            if (s == ","){ // if current token is the end of an expr, then parse the expr and reset the curExpr list
                if (curExpr.last.endsWith("\"")){
                    var str = curExpr.mkString(" ")
                    exprList.append(parseExpr(Array(str), lineNum).get)
                }
                else{
                    exprList.append(parseExpr(curExpr.toArray, lineNum).get)
                }
                curExpr = ArrayBuffer.empty[String]
            }
            else if (s.endsWith(",")){
                curExpr.append(s)
                if (s(s.length-2) == '"'){
                    var str = curExpr.mkString(" ").stripSuffix(",")
                    exprList.append(parseExpr(Array(str), lineNum).get)
                    curExpr = ArrayBuffer.empty[String]
                }
                else if (curExpr(0)(0) != '"'){
                    var str = curExpr.mkString(" ").stripSuffix(",")
                    exprList.append(parseExpr(str.split(" "), lineNum).get)
                    curExpr = ArrayBuffer.empty[String]
                }
            }
            else{
                curExpr.append(s)
            }
        }
        return exprList
    }

    def parseStmt(line: Array[String], lineNum: Int): Option[Stmt] = line match {
        case Array("let", v, "=", rest @ _*) => // Let(variable: String, expr: Expr)
            var expr = parseExpr(rest.toArray, lineNum).get
            return Some(Let(v, expr))
        case Array("print", rest @ _*) => // Print(exprList: List[Expr])
            var exprList = parseStmtPrintHelper(rest.to[ArrayBuffer], lineNum)
            return Some(Print(exprList))
        case Array("if", op1, operator, op2, "goto", label) => // If(expr: Expr, label: String) 
            var expr = parseExpr(Array(op1,operator,op2), lineNum).get
            return Some(If(expr, label+":"))
        case Array("if", op, "goto", label) => // If(expr: Expr, label: String) 
            var expr = parseExpr(Array(op), lineNum).get
            return Some(If(expr, label))
        case Array("input", v) => // Input(variable: String)
            return Some(Input(v))
        case _ => // otherwise, error
            println(s"Syntax error on line $lineNum.")
            System.exit(0)
            None
    }

    def parseExpr(tokens: Array[String], lineNum: Int): Option[Expr] = tokens match{
        case Array(v) => // matches to singular token -> either Constant, Variable, or String
            if (v(0).isLetter){
                return Some(Var(v))
            }
            else if (v(0).isDigit){
                return Some(Constant(v.toDouble))
            }
            else {
                return Some(PrintStr(v.stripPrefix("\"").stripSuffix("\"")))
            }
        case Array(op1, "+", op2) => 
            var e1 = parseExpr(Array(op1), lineNum).get
            var e2 = parseExpr(Array(op2), lineNum).get
            return Some(BinOp("+", e1, e2))
        case Array(op1, "-", op2) => 
            var e1 = parseExpr(Array(op1), lineNum).get
            var e2 = parseExpr(Array(op2), lineNum).get
            return Some(BinOp("-", e1, e2))
        case Array(op1, "*", op2) => 
            var e1 = parseExpr(Array(op1), lineNum).get
            var e2 = parseExpr(Array(op2), lineNum).get
            return Some(BinOp("*", e1, e2))
        case Array(op1, "/", op2) => 
            var e1 = parseExpr(Array(op1), lineNum).get
            var e2 = parseExpr(Array(op2), lineNum).get
            return Some(BinOp("/", e1, e2))
        case Array(op1, "<", op2) => 
            var e1 = parseExpr(Array(op1), lineNum).get
            var e2 = parseExpr(Array(op2), lineNum).get
            return Some(BinOp("<", e1, e2))
        case Array(op1, ">", op2) => 
            var e1 = parseExpr(Array(op1), lineNum).get
            var e2 = parseExpr(Array(op2), lineNum).get
            return Some(BinOp(">", e1, e2))
        case Array(op1, "<=", op2) => 
            var e1 = parseExpr(Array(op1), lineNum).get
            var e2 = parseExpr(Array(op2), lineNum).get
            return Some(BinOp("<=", e1, e2))
        case Array(op1, ">=", op2) => 
            var e1 = parseExpr(Array(op1), lineNum).get
            var e2 = parseExpr(Array(op2), lineNum).get
            return Some(BinOp(">=", e1, e2))
        case Array(op1, "==", op2) => 
            var e1 = parseExpr(Array(op1), lineNum).get
            var e2 = parseExpr(Array(op2), lineNum).get
            return Some(BinOp("==", e1, e2))
        case Array(op1, "!=", op2) => 
            var e1 = parseExpr(Array(op1), lineNum).get
            var e2 = parseExpr(Array(op2), lineNum).get
            return Some(BinOp("!=", e1, e2))
        case _ =>
            println(s"Syntax error on line $lineNum.")
            System.exit(0)
            None
    }

    def perform(stmt: Stmt, output: StringBuilder, symTable: Map[String, Double], lineNum: Int): Option[(StringBuilder, Map[String, Double], Int)] = stmt match{
        case Let(v, expr) => // Let(variable: String, expr: Expr)
            symTable(v) = eval(expr, symTable, lineNum).get.right.get
            return Some((output, symTable, lineNum+1))
        case Print(exprList) => // Print(exprList: ArrayBuffer[Expr])
            for ((expr, i) <- exprList.zipWithIndex){
                if (i == exprList.length-1){
                    var evalOut = eval(expr, symTable, lineNum).get
                    evalOut match {
                        case Left(x) => output.append(x + "\n")
                        case Right(x) => output.append(x.toString + "\n")
                    }
                }
                else{
                    var evalOut = eval(expr, symTable, lineNum).get
                    evalOut match {
                        case Left(x) => output.append(x + " ")
                        case Right(x) => output.append(x.toString + " ")
                    }
                }
            }
            return Some((output, symTable, lineNum+1))
        case If(expr, label) => // If(expr: Expr, label: String)
            if (eval(expr, symTable, lineNum).get.right.get != 0){
                if (symTable.contains(label)){
                    return Some((output, symTable, symTable(label).toInt))
                }
                else{
                    println(s"Illegal goto $label at line $lineNum.")
                    System.exit(0)
                    return None
                }
            }
            else{
                return Some((output, symTable, lineNum+1))
            }
        case Input(v) => // Input(variable: String)
            try{
                var input = readLine().toDouble
                symTable(v) = input
                return Some((output, symTable, lineNum+1))
            }
            catch{
                case _ : Throwable =>
                    println("Illegal or missing input.")
                    System.exit(0)
                    return None
            }
        case No_Op() =>
            return Some((output, symTable, lineNum+1))
    }

    def main(args: Array[String]) {

        // parses input file
    	val infile = args(0)
        val source = scala.io.Source.fromFile(infile)
        val lines = source.mkString.split('\n').map(_.trim)
        source.close()

        // stores tokens in 2d arraybuffer contents
        var contents = new ArrayBuffer[Array[String]]()
        for (l <- lines){
            contents.append(l.split(" "))
        }

        // passes empty ArrayBuffer and empty Map into parseLine
        // parseLine returns a 2-tuple of stmtList: ArrayBuffer[Stmt] and symTable: Map[String, Double].
        var (stmtList, symTable) = parseLine(contents, new ArrayBuffer[Stmt](), scala.collection.mutable.Map[String, Double]())

        var lineNum: Int = 1
        var output: StringBuilder = new StringBuilder()
        stmtList.prepend(No_Op()) // pad for 1-indexing
        while (lineNum < stmtList.length){
            var (output_, symTable_, lineNum_) = perform(stmtList(lineNum), output, symTable, lineNum).get
            output = output_
            symTable = symTable_
            lineNum = lineNum_
        }

        print(output)
    }
}