class Funzioni{                                                                     
    val squared_numbers = (lista : List[Any]) => lista.collect{case x: Int => x * x}
    def symmetric_difference = (l1: List[Int], l2: List[Int]) => l1.diff(l2).concat(l2.diff(l1))
}                                                                                   
object es2 {                                                                        
    def main(args: Array[String]) : Unit = {                                        
    val f : Funzioni = new Funzioni                                                 
    val l1 = List(1,2,3,4,5)                                                        
        val l2 = List(4,5,6,7,8)                                                    
    println("helloworld")                                                           
    println(f.squared_numbers(List(1,"klfdjf", 'c', 283, true, 12)))                
    println(l1.intersect(l2))                                                       
    println(f.symmetric_difference(l1,l2))
}                        
}                        
                         