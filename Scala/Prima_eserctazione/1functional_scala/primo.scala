class Funzioni {
    val isPalindrome = ( s : String ) => {
        val nuova = s.filterNot(x => List(' ',',','?').contains(x)).toUpperCase()
        println(nuova +" =? "+nuova.reverse)
        if (nuova == nuova.reverse)true else false
    }
    def is_an_anagram (s : String, dizionario: List[String]): Boolean = {
        dizionario.foreach(x => 
            if(s.sorted.mkString == x.sorted.mkString) return true)
        return false
    }
    def factors (x:Int, div:Int = 1, res: List[Int] = Nil) : List[Int] = {
        //ritorna una lista di numeri a partire da 1 dove sono solo divisori
        if (div == x) return res
        if((x % div) == 0)   factors(x, div+1, res :+ div) 
        else factors(x,div+1,res)
    }

    val isperfect= (x:Int)  => { factors(x).sum == x }
    
}

object main{
    def main(args : Array[String]) : Unit = {
        val palindrome = List("peppino","xtringa agnirtx","do Geese seegOd?")
        val f = new Funzioni()
        palindrome.foreach(x => println(x +" --> "+ f.isPalindrome(x)))
        val dizionario = List("gatto","scacco","matto","pazzo","tatto","fatto","sacco","patto","cotto","rotto","coatto",
        "tino","cretino","rodino","camino","felino","cricetino","cammino","deficit","ratto","koala","volpe","fiore",
        "madre","sorella","fuoco","rogito","comodo","coccodrillo")
        val controllo = List("torat","ttoma","gotta","roba").foreach(x => println(x+" --> "+f.is_an_anagram(x,dizionario)))
        println("--------------------------------------------------")
        val numeri: List[Int] = List(2,3,4,5,6,7,8,9,345,28,45,999)
        numeri.foreach(x => println(s"$x --> "+ f.factors(x)))
        numeri.foreach(x => println("IS PERFECT: "+x+" -> "+f.isperfect(x)))
    }
}
