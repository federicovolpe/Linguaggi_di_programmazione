//write a program that can evaluate any simple expression          
// allowed operands are: + -- ( and ) and numbers               
import scala.collection.mutable.Stack                                             
                                                                                                       
                                                                                           
object exp_eval {                                                     
    def main(args: Array[String]) : Unit ={                                 
        val exp = "ab3(c2(de))f";                                                                    
        println(ipn2(exp))                                                       
    }                                                                  
                                                                       
    //function that given a string returns the corrisponding expression                             
    //written in inverse polish notation                                                            
    def ipn(exp: String) : String ={                                                                          
        var stack = Stack[Char]()                                                                   
        for( c <- exp){                                                                                                              
            if(c == ')'){ // pop from the stack until (                                               
                var fraParentesi = stack.takeWhile(_ != '(').reverse                                
                while(stack.top != '('){ stack.pop() }                                              
                stack.pop()                                                                         
                var times = stack.pop().toInt - '0'//the next char should be the number                                                                                    
                                                                                                                     
                for(i <- 0 until times){//so add the char to the stack n times                                                   
                    stack.pushAll(fraParentesi)                                                        
                }                                                                                                                                                
            }else{  // put it in the stack                                                                   
                stack.push(c)                                                                                                        
            }                                                                                                
        }                                                                                           
        stack.reverse.mkString                                                                                                      
    }                                                                     
                                                                              
    def ipn2(exp: String): String = {                                         
        def processChar(stack: List[Char], char: Char): List[Char] = {        
            char match {                                                      
                case ')' =>                                                   
                    //divides the stack into two. before the ( and after it                                    
                    val (fraParentesi, rest) = stack.span(_ != '(')           
                    println("fra parentesi: "+fraParentesi +" \n "+ rest)                       
                    val times = rest.tail.head.asDigit                        
                    val repeated = fraParentesi.flatMap(List.fill(times)(_))  
                    println("aggiungo "+fraParentesi+ " "+times+" volte")
                    rest.tail.tail ++ repeated                         
                    println(rest)                                      
                case _ => char :: stack                                       
            }                                                                 
        }                                                                       
        val reversedStack = exp.foldLeft(List.empty[Char])(processChar)       
        reversedStack.reverse.mkString                                        
    }                                                                                           
}                                                                             