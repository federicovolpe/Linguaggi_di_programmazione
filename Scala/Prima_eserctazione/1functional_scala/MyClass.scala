object MyClass {
    def main(args: Array[String]) : Unit = {
        /*val prova : String = "ttoga"
        val lista = List[String] ("ciao", "gatto","mammam", "taco", "rincoglionito", "merda")
        // TEST PALINDROMO
        lista.map(x => f"è palindromo $x ? -> ${is_palindrome(x)}\n").foreach{print}
        // TEST ANAGRAMMI
        lista.map(x => f"anagrammi $x e $prova ? -> ${is_an_anagram(x,prova)}\n").foreach{print}
        // TEST FATTORI
        var risultato : Array[Int] = Array(1)
        factors(14, 14, 2, risultato).foreach{println}
        println("fattori2:")
        factors2(496).foreach{println}*/
        for(i <- 0 to 1200000){
            is_proper(i)
        }
    }
    val is_palindrome = (s : String) => {
        val s1 = s.filterNot(x => List('.',',',' ','?',';').contains(x)).toUpperCase()
        s1.equals(s1.reverse)
    }

    val is_an_anagram = (s1 : String, s2: String) => { s1.toList.sorted.equals(s2.toList.sorted) }
    
    def factors (orig: Int,prog: Int, divisore: Int, fattori: Array[Int]): Array[Int] = {
        if (divisore > orig || prog == 0){
            fattori
        } else { 
            if (prog % divisore == 0){
                factors(orig, prog/divisore, divisore, fattori:+divisore)
            } else {
                factors(orig, prog , divisore+1, fattori)
            }
        }
    }
    def factors2 (num: Int, divisore: Int = 2, fattori :List[Int] = Nil) : List[Int] = {
        LazyList
        .iterate(divisore)(i => i+1) //crea una lista che parte dal divisore allinfinito
        .takeWhile(n => n <= num) //seleziona i numeri che sono inferiori al target
        .find(n => num % n == 0 ) //seleziona i numeri che sono divisori del numero
        .map(n => factors2(num/n, n, fattori :+ n)) //viene fatta la chiamata con il numero diviso per il divisore trovato 
        .getOrElse(fattori) 
    }
    def is_proper (n:Int): Unit = {
        if (divisors(n).sum == n) {
            println(s"$n è perfetto")
        }
    }
    def divisors(n: Int, divisore : Int = 1, res : List[Int] = Nil): List[Int] = {
        if(divisore >= n){
            res
        }else if(n % divisore == 0){
            divisors(n, divisore+1, res:+divisore)
        }else{
            divisors(n, divisore+1, res)
        }
    }
}
