class Funzioni {
  val even = (x : Int) => x % 2 == 0 
  def presentePari (x : List[Int]): Boolean = {
    x match{
        case Nil => false
        case ((h: Int) :: (t: List[Int])) => if (h % 2 == 0) {return true} 
                                            else {presentePari(t)} 
    }
  }
  def luckySeven (x : List[Int], sum : Int = 0): Int = {
    x match{
        case Nil => sum
        case ((h: Int) :: (t: List[Int])) =>
            if (h == 7){ 
            luckySeven(t, sum + 14)}
            else { luckySeven(t, sum + h)}
    }
  }
  def balance(x : List[Int], indice : Int = 1): (Boolean,Int) = {
    if (indice > x.size) {(false,-1)} 
    else if (x.splitAt(indice)._1.sum == x.splitAt(indice)._2.sum) {(true, indice)} 
    else {balance(x, indice+1 )}
  }
}

object main{
    def main(args: Array[String]): Unit = {
        val f : Funzioni = new Funzioni
        val lista = List(1,2,3,4,5,6,7,8,9)
        val lista2 = List(1,7,3,4,1)
        //lista.foreach(x => println(f.even(x)))
        //println(f.presentePari(lista))
        //println(f.luckySeven(lista))
        println(f.balance(lista2))

    }
}
