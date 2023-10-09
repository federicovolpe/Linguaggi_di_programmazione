class Funzioni {
  def conta (x : Array[Char], y : Char, contatore : Int = 0) => x match{

  }
}
/*Write a Scala program to check whether a given character presents 
in a string between 2 to 4 times*/
object main{
    def main(args: Array[String]) : Unit ={
        val esempio : String = "fancoolo panzone"
        val lettera : Char = "c"
        val arrayEsempio : Array[Char] = esempio.split()
        
        println(Funzioni.conta(arrayEsempio, lettera) > 2 && Funzioni.conta(arrayEsempio, lettera) < 4 )
    }
}
