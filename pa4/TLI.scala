import scala.collection.mutable.Map
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import util.control.Breaks._

abstract class Expr
case class Var(name: String) extends Expr
case class Str(name: String) extends Expr
case class Constant(num: Double) extends Expr
case class BinOp(operator: String, op1: Expr, op2: Expr) extends Expr

abstract class Stmt
case class Let(variable: String, expr: Expr) extends Stmt
case class If(expr: Expr, label: String) extends Stmt
case class Input(variable: String) extends Stmt
case class Print(exprList: List[Expr]) extends Stmt
case class No_Op() extends Stmt

object TLI {
    def eval(expr: Expr, symTable: Map[String, Double]): Double = expr match {
        case BinOp("+",e1,e2) => eval(e1,symTable) + eval(e2,symTable) 
        case BinOp("-",e1,e2) => eval(e1,symTable) - eval(e2,symTable) 
        case BinOp("*",e1,e2) => eval(e1,symTable) * eval(e2,symTable) 
        case BinOp("/",e1,e2) => eval(e1,symTable) / eval(e2,symTable) 
        case BinOp("<",e1,e2) => {
            if (eval(e1,symTable) < eval(e2,symTable)){
                1
            }
            else{
                0
            }
        }
        case BinOp(">",e1,e2) => eval(e1,symTable) - eval(e2,symTable) 
        case Var(name) => symTable(name)
        case Constant(num) => num
	    case _ => {
            println("error")
            0
        } // should really throw an error
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
                if(line(0).endsWith(':')){
                    symTable(line(0)) = lineNum
                    statement = parseStmt(line.slice(1, line.length-1), lineNum)
                }
                else{
                    statement = parseStmt(line, lineNum)
                }

                stmtList.append(statement)
            }
            
        }
        
        (stmtList, symTable)
    }

    def parseStmt(line: Array[String], lineNum: Int): Stmt = {

    }

    def parseExpr(tokens: Array[String], lineNum: Int): Expr = {

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
        // parseLine returns a 2-tuple of stmtList and symTable.
        var (stmtList, symTable) = parseLine(contents, new ArrayBuffer[Stmt](), scala.collection.mutable.Map[String, Double]())

        println(stmtList)
        println(symTable)

        // for (line <- contents){
        //     println(line.length)
        //     for (word <- line){
        //         print(word + " ")
        //     }
        //     println()
        // }
    }
}