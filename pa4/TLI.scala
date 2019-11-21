import scala.collection.mutable.Map

abstract class Expr
case class Var(name: String) extends Expr
case class Str(name: String) extends Expr
case class Constant(num: Double) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr

abstract class Stmt
case class Let(variable: String, expr: Expr) extends Stmt
case class If(expr: Expr, label: String) extends Stmt
case class Input(variable: String) extends Stmt
case class Print(exprList: List[Expr]) extends Stmt

object TLI {
    def eval(expr: Expr, symTab: Map[String, Double]): Double = expr match {
        case BinOp("+",e1,e2) => eval(e1,symTab) + eval(e2,symTab) 
        case Var(name) => symTab(name)
        case Constant(num) => num
	case _ => 0 // should really throw an error
    }

    def main(args: Array[String]) {
    	println("this needs completing")
    }
}