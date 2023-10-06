class Matrice (val mat : List[List[Int]]) {
    def toString(m : List[List[Int]] = this.mat, finale : String = ""): String = {
        m match{
            case ((h : List[int]) :: t) => toString(t,finale.concat(h.toString + "\n"))
            case Nil => return finale
        } 
    }
    def eq (b: Matrice): Boolean = {
        //if(this.mat.length = b.length && this.mat(1).length == b(1).length){}
        (this.mat, b.mat) match {
            case (Nil, Nil) => return true
            case ((x: List[Int]) :: tx, (y :List[Int]) :: ty) => (x,y) match{
                //controllo se la listax e la lista y sono uguali
                case i if x.equals(y) => {println("lista uguale")
                                            tx.eq(ty)}
                case _ => return false
            }
            //caso di default ovvero quando qualcosa non va bene
            case _ => return false
        }
    }
    def copy (b: Matrice): Unit = {}

    def add (a : Matrice = new Matrice(this.mat) ,b : Matrice): List[List[Int]] = {
        if (a.mat.isEmpty || b.mat.isEmpty) return Nil
        if (a.mat.size != b.mat.size || a.mat.head.size != b.mat.head.size) return List(List(0))
        (a.mat.head zip b.mat.head)
        .map{case (x, y) => x + y } :: a.add(new Matrice(a.mat.tail),new Matrice(b.mat.tail))
    }

    def scalar (n : Int): List[List[Int]] = this.mat.map(x => x.map(y => y * n))

    def mult (b: Matrice): List[Int] = {
        if(this.mat.size == 0 || b.mat.size == 0) return Nil

        b.mat.map{case riga: List[Int] => 
            println("matrici :\n"+this.mat.toString() + "\n"+b.mat.toString())
            println("moltiplico" + this.mat.head + " per " + riga + " = " + multvettore(this.mat.head, riga).sum)
            return List(multvettore(this.mat.head, riga).sum :: (new Matrice(this.mat).mult(new Matrice(b.mat.tail))))
        }
    }
    def multvettore (a: List[Int], b : List[Int]) : List[Int] = (a zip b).map{case (x,y) => x * y}
    def transpose (): List[List[Int]]= {this.mat.transpose}
    def norm (): Unit = {}
    val size = () => this.mat.length
    val get = (x : Int, y : Int) => this.mat(x)(y)
    
}

object main{
    def main (args: Array[String]) : Unit = {
        val test = List(List(1,2,3),List(4,5,6),List(7,8,9))
        val a = new Matrice(test)
        val b = new Matrice(List(List(1,2,4),List(4,5,6),List(7,8,9)))
        println(s"a.lenhth = ${a.size()}" )
        println(s"a [1][1] = ${a.get(1,1)}" )
        println(s"a == b? = ${a.eq(b)}" )
        val somma = new Matrice(a.add(a,b))
        println("somma di a e b = \n"+ somma.toString(somma.mat))
        val scalare = new Matrice(a.scalar(5))
        //println("a scalare 5 = \n" + scalare.toString(scalare.mat))
        //println(a.mult(a))
        //val moltiplicazione = new Matrice(a.mult(a))
        println("a moltiplicato per a =\n")
        a.mult(new Matrice(a.transpose()))
    }
}
