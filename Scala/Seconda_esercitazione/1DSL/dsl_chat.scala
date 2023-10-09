import scala.math._

object CalculatorDSL {
  sealed trait Expr
  case class Number(value: Double) extends Expr
  case class Add(left: Expr, right: Expr) extends Expr
  case class Subtract(left: Expr, right: Expr) extends Expr
  case class Multiply(left: Expr, right: Expr) extends Expr
  case class Divide(left: Expr, right: Expr) extends Expr
  case class Power(base: Expr, exponent: Expr) extends Expr
  case class Sqrt(value: Expr) extends Expr
  case class Sin(value: Expr) extends Expr
  case class Cos(value: Expr) extends Expr
  case class Tan(value: Expr) extends Expr
                                       
  def eval(expr: Expr): Double = expr match {
    case Number(value) => value        
    case Add(left, right) => eval(left) + eval(right)
    case Subtract(left, right) => eval(left) - eval(right)
    case Multiply(left, right) => eval(left) * eval(right)
    case Divide(left, right) => eval(left) / eval(right)
    case Power(base, exponent) => pow(eval(base), eval(exponent))
    case Sqrt(value) => sqrt(eval(value))
    case Sin(value) => sin(eval(value))
    case Cos(value) => cos(eval(value))
    case Tan(value) => tan(eval(value))
  }                                    
                                       
  def evaluate(script: String): Unit = {
    val lines = script.split('\n')     
    for (line <- lines) {              
      val expr = parseExpr(line)       
      val result = eval(expr)          
      println(result)                  
    }             
  }                   
                      
  def parseExpr(input: String): Expr = {
    def tokenize(str: String): List[String] = {
      str             
        .replace("(", " ( ")                               
        .replace(")", " ) ")                               
        .split("\\s+")                                     
        .filter(_.nonEmpty)                                
        .toList                                            
    }                                                                           
                                                                                
    def parse(tokens: List[String]): (Expr, List[String]) = tokens match {      
      case Nil => throw new IllegalArgumentException("Invalid expression")      
      case ")" :: rest => throw new IllegalArgumentException("Unbalanced parentheses")
      case "(" :: rest =>                                                       
        val (expr, remainingTokens) = parse(rest)                               
        remainingTokens match {                                                 
          case ")" :: more => (expr, more)                                      
          case _ => throw new IllegalArgumentException("Unbalanced parentheses")
        }                                                                       
      case numberStr :: rest =>                                                 
        val expr = try {                                                        
          Number(numberStr.toDouble)                                            
        } catch {                                                               
          case _: NumberFormatException =>                                      
            numberStr.toLowerCase match {                                       
              case "sqrt" =>                                                    
                val (value, remainingTokens) = parse(rest)                      
                (Sqrt(value), remainingTokens)                                  
              case "sin" =>   
                val (value, remainingTokens) = parse(rest)
                (Sin(value), remainingTokens)
              case "cos" =>                
                val (value, remainingTokens) = parse(rest)
                (Cos(value), remainingTokens)
              case "tan" =>                
                val (value, remainingTokens) = parse(rest)
                (Tan(value), remainingTokens)
              case _ => throw new IllegalArgumentException("Invalid expression")
            }                              
        }                                  
        parseSubExpr(expr.asInstanceOf[Expr], remainingTokens)
    }                                      
                                           
    def parseSubExpr(expr: Expr, tokens: List[String]): Expr = tokens match {
      case Nil => expr                     
      case "+" :: rest =>                  
        val (right, remainingTokens) = parse(rest)
        parseSubExpr(Add(expr, right), remainingTokens)
      case "-" :: rest =>                  
        val (right, remainingTokens) = parse(rest)
        parseSubExpr(Subtract(expr, right), remainingTokens)
      case "*" :: rest =>            
        val (right, remainingTokens) = parse(rest)
        parseSubExpr(Multiply(expr, right), remainingTokens)
      case "/" :: rest =>          
        val (right, remainingTokens) = parse(rest)
        parseSubExpr(Divide(expr, right), remainingTokens)
      case "^" :: rest =>          
        val (right, remainingTokens) = parse(rest)
        parseSubExpr(Power(expr, right), remainingTokens)
      case ")" :: _ => expr        
      case _ => throw new IllegalArgumentException("Invalid expression")
    }                              
                                   
    val tokens = tokenize(input)   
    val (expr, remainingTokens) = parse(tokens)
    if (remainingTokens.nonEmpty) {
      throw new IllegalArgumentException("Invalid expression")
    }                              
    expr                           
  }                                
}                                  
                                   