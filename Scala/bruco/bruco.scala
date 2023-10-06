// write an arithmetic evaluator using the stack for an expression             
import scala.util.parsing.combinator._                                   
                                                         
// main goal is to execute the code given by the compiler                                                         
object environment{                                                                            
    var (x, y) = (0,0)                                                                               
                                                                                               
    def execute(l: List[Char]) : (Int, Int) = {                                                
        l.foreach{ command =>        
            command match {                                                             
                case '↑' => y += 1                                                                    
                case '↓' => y -= 1                                                                    
                case '←' => x -= 1                                                                   
                case '→' => x += 1                                                                   
                case _ => throw new IllegalArgumentException("trovato un simbolo non corretto")
            }                                                                                  
        }                                                                                      
        return (x,y)                                                                                    
    }                                                                                           
}                                                                                              
                                                                                                   
//main goal is to compile the parenthesis of the code                                                                       
object brucoCompiler{                                                                                                                                              
                                                                                                                            
    //objective: recursively compile a parenthesis                                                                          
    def compile(l: List[Char], n: Int = 0 , done: List[Char] = Nil) : (List[Char], List[Char]) = {                                                                                  
                                                                                           
        val symbols = List('↑','↓','←','→','x','y')                                                                                                      
        l match{                                                                                                       
            case num :: _ if Character.isDigit(num) => //in case of a number a new expression is beginning                                                        
                // solve the parenthesis                                                   
                                                                                                                     
                val (parentesiRisolta, resto) = compile(l.tail.tail, num.toInt - '0'.toInt)    
                println("- finita la chiamata ricorsiva n = "+ n)                                                           
                //need to know where the parenthesist is closed                                
                val result = done ++ parentesiRisolta ++ compile(resto)._1                     
                println("- - finita la seconda chiamata ricorsiva n = "+ n)                                     
                println("dopo somma delle parentesi: " + result)                                                            
                (result, Nil)                                                                                                      
                                                                                                                            
            case ')' :: tail => //moltiplico per il numero e ritorno
                println("\nn = " +n+"\n")                                                                  
                val repeated : List[Char] = List.fill(n)(done).flatten                                                      
                println("ho ripetuto : "+ repeated + " " + n+"volte")                                   
                //return the tuople of the executed parenthesis and the rest of the expression                                                              
                (repeated, tail)                                                                                            
                                                                                                                            
            case x :: tail if symbols.contains(x) =>                                           
                                                                                                                    
                println("aggiunto "+ x +" a "+ done)                                                                        
                compile(tail, n, done :+ x)                                                    
                                                                                               
            case _ => (done, Nil)                                                                          
        }                                                                                
    }                                                                                
}                                                                                    
                                                                                     
//object main objective is to transform the expression into a stack of char          
object brucoParser extends JavaTokenParsers{                                                                   
    def comandi: Parser[List[Char]] = rep1(elem("character", _ => true))             
                                                                                                         
}                                                                                    
                                                                                                               
object main{                                                                                                   
    def main(args: Array[String]) : Unit = {                                                                   
        //reading from file                                                                                    
        args.foreach{ nomefile =>                                                                              
            val espressione = io.Source.fromFile(nomefile).mkString                                                
                                                                                                               
            //using the parser                                                                                 
            brucoParser.parseAll(brucoParser.comandi, espressione) match {                                                 
                case brucoParser.Success(res, _) =>   println("tradotto: "+ res)
                    //val resu = environment.execute(                          
                        println("compilazione: " + brucoCompiler.compile(res))
                    //println("posizione finale: ("+ resu._1 + " , " +resu._2 +")")
                case brucoParser.NoSuccess(err, _) =>                          
                    println("errore nel parsing: "+ err)                       
            }                                                                                                  
        }                                                                                                      
    }                                                                                                          
}                                                                                                              