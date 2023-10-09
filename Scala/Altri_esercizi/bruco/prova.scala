                                                      
object main{                                          
    def main(args: Array[String]): Unit ={            
        val a = Array(1,2,3)                          
        println("res : "+ plusOne(a))                                     
    }                                                 
                                                      
    def plusOne(digits: Array[Int]): List[Int] ={                                                  
                                                
        def sum(ns: List[Int]): List[Int] = {   
            ns match{                           
                case 9 :: tail =>               
                            0 :: sum(tail)        
                                                  
                case n :: tail =>               
                            //println("ho trovato ")                
                            n+1 :: tail             
                                                  
                case _ => ns :+ 1                 
            }                                     
        }                                         
                                                  
        sum(digits.reverse.toList).reverse      
    }                                             
}                                                 