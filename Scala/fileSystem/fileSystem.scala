//the operations of the filesystem
// rename                         
// mkdir                          
// mkfile                         
// move                           
// see
// cd
// print
import scala.util.parsing.combinator._

sealed trait Entry
case class File (name: String) extends Entry
case class Directory (name: String, entries: List[Entry]) extends Entry

sealed trait Comando
case class Rename (fileName: String, newName: String) extends Comando
case class Mkdir (name: String) extends Comando
case class Mkfile (name: String) extends Comando
case class Move (name: String, destination: String) extends Comando
case class See (fileName: String) extends Comando
case class Cd (directoryName: String) extends Comando
case class Print () extends Comando
                  

object fInterpreter{
    var env = List[Entry]()

    def execute(comandi: List[Comando]): Unit = comandi match {
        case h :: t =>
            h match{
                case h: Mkdir => mkdir(h.name)
                case h: Cd => cd(h.directoryName)
                case h: Mkfile => mkfile(h.name)
                case h: Move => move(h.name, h.destination)
                case h: Print => print
            }
            execute(t)
        case _ => 
    }

    def mkdir(name: String): Unit = {
      println("creo la nuova cartella: "+name)
    }

    def cd(name: String): Unit = {
      if(env.exists(x => x.name == name)){
            println("mi muovo nella cartella: "+ name)
        }else{
          println("cd non terminato, "+ name +" non è stato ritrovato nella directory corrente")
        }
    }

    def mkfile(name: String): Unit = {
        env = File(name) :: env
    }

    def move(name: String, destination: String): Unit = {
        println("non so ancora fare la move") 
    }

    def print: Unit = {
        println("stampa del fileSystem")
        env.foreach{println}
    }
}

object fParser extends JavaTokenParsers{
    def comandi:(Parser[List[Comando]]) = rep1(comando) 

    def comando:(Parser[Comando]) = 
        ((("mkdir" | "cd" | "mkfile") ~ ident) 
        | ("move" ~ ident ~ ident) 
        | "print") ^^{
        case "mkdir"  ~ n => 
            println("trovato un mkdir") 
            Mkdir(n.toString)
        case "cd"     ~ n =>
            println("trovato un cd")
            Cd(n.toString)
        case "mkfile" ~ n =>
            println("trovato un mkfile")
            Mkfile(n.toString)
        case "move" ~ n ~ d => 
            println("trovato un move")
            Move(n.toString, d.toString)
        case "print" => 
            println("trovato un print")
            Print()
        case x => throw new IllegalArgumentException("comando non riconosciuto : "+x)
    }
}                              
                                  
object main{                      
    def main(args: Array[String]){
        args.foreach{filename =>
            val testo = io.Source.fromFile(filename).mkString
            fParser.parseAll(fParser.comandi, testo) match {
                case fParser.Success(res,_) =>
                    println("successo : \n" + res.foreach{println})
                    println("di lunghezza: "+ res.length)
                    fInterpreter.execute(res)

                case fParser.NoSuccess(err,_) => 
                    println("errore : " + err)
            }
        }                       
    }                           
}                                 
