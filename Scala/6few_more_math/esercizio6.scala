class Ft {

    def primo (x : Int, divisore : Int = 2) : Boolean = {
        //stabilisce se un numero è primo
        if (divisore * divisore > x){ true}
        else if (x % divisore == 0){ false}
        else (primo(x, divisore+1))
    }

    def goldbach (x : Int) : List[Int] = {
        //se il numero risulta uguale alla somma di due primi allora ritorna la lista
        for (i <- 2 until x) {
    // If i is prime and x-i is prime, return the list
    if (primo(i) && primo(x-i)) return List(i, x-i)
    }
    // if no prime numbers are found that add up to x
    Nil
    }
    def goldbach_list (n : Int, m : Int): Unit = {
        for (i <- n until m){
            println(goldbach(i))
        }
    }
}

object main{
    def main(args: Array[String]): Unit = {
       val f : Ft = new Ft
       val lista :  List[Int] = List(3,4,5,6,7,8,13981,483974,3248,934)
       lista.foreach{x => println(x ," ", f.goldbach(x))}
       f.goldbach_list(1 , 100)
    }
}


