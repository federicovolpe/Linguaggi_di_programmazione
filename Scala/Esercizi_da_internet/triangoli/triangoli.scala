//make a program that reads three angles of triangle and can tell if they are     
//isoscele, scaleno, equilater                                                    
import scala.util.parsing.combinator._                                            
                                                                 
sealed trait Triangolo                                                                                
case class Triangle(a: Int, b: Int, c: Int) extends Triangolo                 
case class Rettangolo(a: Int, b: Int, c: Int) extends Triangolo               
case class Ottusangolo(a: Int, b: Int, c: Int) extends Triangolo              
case class Scaleno(a: Int, b: Int, c: Int) extends Triangolo                  
case class Equilatero(a: Int, b: Int, c: Int) extends Triangolo               
case class Unvalid(a: Int, b: Int, c: Int) extends Triangolo                  
                                                                              
object tClassifier{                                                           
    def classify(triangles: List[Triangle]):List[Triangolo] = triangles match{
        case h :: t =>                                         
            if(h.a + h.b + h.c != 180) {                       
                Unvalid(h.a , h.b , h.c) :: classify(t)        
            }else if(h.a == 90 || h.b == 90 || h.c == 90){     
                Rettangolo(h.a , h.b , h.c) :: classify(t)                    
            }else if(h.a > 90 || h.b > 90 || h.c > 90){        
                Ottusangolo(h.a, h.b, h.c) :: classify(t)                    
            }else if(h.a == 60 && h.b == 60 && h.c == 60){    
                Equilatero(h.a, h.b, h.c) :: classify(t)                  
            }else{                                         
                Scaleno(h.a, h.b, h.c) :: classify(t)     
            }                                             
        case _ => Nil                         
    }                                                          
}                                                                             
                                                                                  
object tParser extends JavaTokenParsers{                                          
    def triangles:(Parser[List[Triangle]]) = rep1(triangle)                       
                                                                                  
    def triangle:(Parser[Triangle]) = wholeNumber ~ wholeNumber ~ wholeNumber ^^ {
        case a ~ b ~ c => Triangle(a.toInt, b.toInt, c.toInt)                          
    }                                                                                  
}                                                                                       
object main {                                                                           
    def main(args : Array[String]) : Unit = {                                           
        //read the file                                                                 
        args.foreach{ filename =>                                                       
            val testo = io.Source.fromFile(filename).mkString                           
            tParser.parseAll(tParser.triangles, testo) match {                          
                case tParser.Success(res,_) =>                                          
                println("success: \n"+ res.foreach{println})                            
                println("classificati: \n"+ tClassifier.classify(res).foreach{println})
                                                                                       
                case tParser.NoSuccess(err,_) => println("errore: "+ err)               
            }                                                                           
                                                                                        
        }                                                                               
    }                                                                                   
}                                                                                       