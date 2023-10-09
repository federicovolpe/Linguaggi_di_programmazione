import scala.collection.immutable.StringOps
class KWIC {
    val nonvolute = List("the", "of", "But", "over", "about", "for")
    def explode_title (title : String, index: Int) = {
        val title_words = title.split(" ")
        (0 to (title_words.size-1))
            .filter(x => !nonvolute.contains(title_words(x).toLowerCase))
            //divido la stringa in due(prima dell'interessata, dopo l'interessata)
            .map(x => title_words.splitAt(x))
            .map({case (l,r) => (l.mkString(" "), r.mkString(" "))})
            .map({case (l,r) => f"${index+1}%4d ${l.substring(Math.max(l.length(),33)-33)}%33s ${r.substring(0,Math.min(r.length(),40))}%-40s"})
    }

    def calc(titles : Iterator[String]) = {
        titles
        .zipWithIndex //suddivide i titoli, ciascuno con un indice
        //creo una lista di liste (ognuna ha il titolo ripetuto n parole volte)
        .map({case(title, index) => explode_title(title, index)})
        .flatten //per ritornale da liste di liste di string a liste di stringhe
        .toList //da iteratore torna lista
        .sortWith((t1,t2) => t1.substring(39) < t2.substring(39))
        //li ordino in base alla lettera che sta alla trentanovesima posizione
    }   
}
object KWIC{
    def main(args: Array[String]): Unit = {
        println(6040 % 18)
        println(6140 % 18)
        println(6240 % 18)
        println(6340 % 18)
        println(6440 % 18)
        println(6540 % 18)
        println(6640 % 18)
        println(6740 % 18)
        println(6840 % 18)
        /*val t = new KWIC
        args.foreach {
            filename =>
            val titles = scala.io.Source.fromFile(filename, "ISO-8859-1").getLines()
            val the_kwic = t.calc(titles)
            the_kwic.foreach(println)
        }*/
    }
}
