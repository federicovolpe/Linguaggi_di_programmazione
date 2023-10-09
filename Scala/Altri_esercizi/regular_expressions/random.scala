object main{
  def main(args: Array[String]): Unit = {
    //lettura dei due numeri in input
    val list = List(1,2,3,4,16,6)
    val t = list.exists(x => list.exists(y => trenta(x,y)))

    println(t)
    println(scambia("scambia"))
    println("terzo : "+ primidue("primidue"))
    println("quarto : " + indice("io ho fatto la cacca ieri", "ca"))
    println("quinto : " + upperlast("soprapanca", 5))
  }



  def trenta(x: Int, y : Int) : Boolean = {
      x > 30 || y > 30 || (x + y) > 30 
  }
  
  //funzione che ha lo scopo di scambiare il primo e ultimo carattere di una stringa
def scambia(s: String): String = s match {
  case "" => throw new IllegalArgumentException("La stringa da scambiare non può essere vuota")
  case head if head.length == 1 => s // String with only one character remains unchanged
  case head => s.last + s.tail.init + s.head
}

    def primidue(s: String): String ={ 
        val primo = s.head
        val secondo = s.tail.head
        val primi = primo.toString + secondo.toString
        primi * 4
    }
    

    //funzione che ritorna l'indice di una sottostringa x
    def indice(stringa: String, substringa: String, i: Int = 0): Int ={
        if(stringa.length < substringa.length) 0

        if(stringa.substring(0, substringa.length) == substringa){
            i 
        }else{
          indice(stringa.tail, substringa, i+ 1)
        }
    } 

    def upperlast(s: String, n: Int): String ={
      if(s.length == n) {
          s.toUpperCase 
      }else{
          s.head + upperlast(s.tail, n)
      }
    }
}
