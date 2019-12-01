import scala.collection.mutable.Map
import scala.io.Source
import scala.collection.mutable.ArrayBuffer

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

        for((line, i) <- lines.zipWithIndex){
            stmtList.append()
        }

        
    }

    def main(args: Array[String]) {

        // parses input file
    	val infile = args(0)
        val source = scala.io.Source.fromFile(infile)
        val lines = try source.mkString.split('\n').map(_.trim) finally source.close()

        // stores tokens in 2d arraybuffer contents
        var contents = new ArrayBuffer[Array[String]]()
        for (l <- lines){
            contents.append(l.split(" "))
        }
        
        var stmtList = new ArrayBuffer[Stmt]()
        var symTable = scala.collection.mutable.Map[String, Double]()

        var parseLineOut = parseLine(contents, stmtList, symTable)
        stmtList = parseLineOut._1
        symTable = parseLineOut._2

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