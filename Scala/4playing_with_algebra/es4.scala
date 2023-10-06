/*le classi implementate devono contenere delle funzioni che controllino 
che le proprietà delle varie strutture algebriche vengano rispettate*/

class monoid{
  val operazione = (a : Int, b : Int ,f: (Int,Int) => Int) => f(a,b)
  val a : Int  
  val b : Int
  def chiusura (): Boolean = {
    // per ogni elemento a,b op(ab è ancora nell'insieme)
  
  }
  def associatività () : Boolean = {
    //dati a, b, c appartenenti a M, vale (ab)c = a(bc)
  }
  def nullo () : Boolean = {
    // esiste e tale che op(e,a) = op(a,e) = a
  }
}

class group{
  def associatività() :Boolean = {}
  //prec
  def neutro () : Boolean = {
    // esiste e tale che op(e,a) = op(a,e) = a
  }
  def inverso () : Boolean = {
    // a*a' = a'*a = e
  }
}

class ring{
  val op1 = 0
  val op2 = 1
  def associatività (op1) : Boolean = {
    //dati a, b, c appartenenti a M, vale (ab)c = a(bc)
  }
  def commutatività (op1) : Boolean = {

  }
  def 
}

object es4 {
  def main(args: Array[String]) : Unit = {
    println("hellowoerld")
  }
}
//Users/luigivolpe/Downloads/akka-quickstart-scala/sbt-dist/bin/java9-rt-export-0.1.0.jar