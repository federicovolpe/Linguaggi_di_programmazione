class Funzioni{
    val is_palindrome = (s : String) => {
            val tmp = s.filterNot(x => List('.',',','?',' ','!').contains(x)).toLowerCase
            tmp == tmp.reverse
        }
    val anagram = (s1 : String, s2 : String) => (s1.diff(s2).size == 0) && (s2.diff(s1).size == 0)
    val is_an_anagram = (s:String, slist : List[String]) => slist.map(x => anagram(s,x)).contains(true)
    def primo (x : Int , divisore: Int = 2):Boolean= {
        if((divisore * divisore) > x){return true}
        if(x % divisore == 0) {return false} else {primo(x,divisore+1)} 
    }
    def factors (x : Int , divisore : Int = 2, fattori: List[Int] = Nil): List[Int] = {
        if(divisore > x){ return fattori.filter(x => primo(x))}
        if(x % divisore == 0){factors(x,divisore+1,fattori :+ divisore)} else {factors(x,divisore+1,fattori)}
    }
    val is_perfect = (n: Int) => ((2 until n).filter{x => n % x == 0}).sum == (n-1)
        //((2 until n).collect{case x if n % x == 0 => x}).sum == (n-1)
}

object es1{
    def main(args: Array[String]): Unit = {
        val f :Funzioni = new Funzioni
        
        
        val dizionario = List("mamma","gatto","ttoga","casa","saca","puttana",
        "fancoolo","martina","noname","nonei","maneno","bingus","criceto","rame","computer",
        "scacco","saccoccia","nome","cognome")
        val stringhe = List("ammam","minemi","togga","cascco","roberto")
        //.map(x => println(s"$x ${f.is_an_anagram(x,dizionario)}"))
        val listnumeri = List(2,3,4,5,6,7,8,9,28,983,32,49,65)
        .foreach(x => println(s"$x --> ${f.is_perfect(x)}"))
        //.foreach(x => println(s"$x --> ${f.factors(x)}"))
    }
}