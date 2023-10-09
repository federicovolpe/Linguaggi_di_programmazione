import scala.util.parsing.combinator._                                                                                                                                
                                                                                                                                                                      
sealed trait Comando                                                                                                                                                  
//i path in questo caso sono composti da un singolo nome quindi string, altrimenti devono essere cambiati in List[String]                                             
case class Ls            () extends Comando                                                                                                                           
case class Cd   (p: String) extends Comando                                                                                                                           
case class Touch(p: String) extends Comando                                                                                                                           
case class Mkdir(p: String) extends Comando                                                                                                                           
                                                                                                                                                                      
object TParser extends JavaTokenParsers{                                                                                                                              
    def parse: Parser[List[Comando]] = rep1(comando) 
                                                                                                                                                                                      
    //parser per un comando                                                                                                                                                           
    def comando : Parser[Comando] = {                             
        ("touch" | "cd" | "mkdir") ~ ident ^^{                                                                                                                                                                             
            case c ~ p => c match{                                                                                                                                                        
                                case "touch" =>                                                                                                                                           
                                    println("riconosciuto il comando touch " + p)                                                                                                         
                                    Touch(p)                                                                                                                                         
                                case "cd"    =>                                                                                                                                           
                                    println("riconosciuto il comando cd " + p)                                                                                                            
                                    Cd(p)                                                                                                                                                 
                                case "mkdir" =>                                                                                                                                           
                                    println("riconosciuto il comando mkdir " + p)                                                                                                                
                                    Mkdir(p)                                                                                                                                                     
                                }                                                                                                                                                                 
            case _ => throw new java.lang.IllegalArgumentException("errore nel parser comando")                                                                                                   
        }    |                                                       
        "ls" ^^ {case c => println("riconosciuto il comando ls ")                                                                                                            
                                    Ls()                            
                   }                                                
    }                                                                                                                                                                                          
}                                                                                          
                                                                                           
//interprete dei comandi ha il compito di ricevere i comandi e redirezionarli al filesystem   
object TerminalInterpreter{                                                                
    def execute (comandi : List[Comando], f : FileSystem) = {       
        comandi.foreach{ c => c match {                             
                case Mkdir(n) => f.mkdir(n)                         
                case Cd(n)    => f.cd(n)                            
                case Ls()     => f.ls()                                               
                case Touch(n) => f.touch(n)                                           
            }                                                                         
        }                                                                             
    }                                                                                 
}                                                                                          
                                                                                           
trait Entry {                        
    val nome : String                
    def print()                      
}                                    
                                     
class File(override val nome: String) extends Entry {
    override def print() ={            
        println("file: ------> " + nome)                     
    }                                                              
}                                                                  
                                                                   
class Directory(override val nome: String,var entries: List[Entry] = List.empty) extends Entry{                      
    def getEntry(nome: String): Option[Entry] = entries.find(_.nome == nome)
                                                                   
    def newdir(nome: String) = {                                   
        val newDirectory = new Directory(nome)                     
        entries = newDirectory :: entries                          
    }                                                              
                                                                   
    def newfile(nome: String) = {                       
        val newFile = new File(nome)                               
       entries = newFile :: entries                                        
    }                                                              
                                                                   
    override def print() = {                                     
        println("Directory: " + nome)  
        entries.foreach{x => x.print()}                            
    }                                                                                    
}                                                                                           
                                                                                            
class FileSystem{                                                                           
    val root : Directory = new Directory("root", List())                                                                
    var current : Directory = root                                                             
                                                                                            
    def cd (nome: String) = {                                                               
        println("eseguo cd " + nome)                                                        
        current.getEntry(nome) match {                                                      
        case Some(directory: Directory) => current = directory                              
        case _                          => println(s"Directory '$nome' not found.")         
        }                                                                                   
    }                                                                                       
    def mkdir(nome: String) = {                                                             
        println("eseguo mkdir " + nome)                                                        
        current.newdir(nome)                                                                
    }                                                                                       
    def touch(nome: String) = {                                                             
        println("eseguo touch " + nome)                                                        
        current.newfile(nome)                                                               
    }                                                                                      
    def ls() : Unit = {                                                                                    
        println("eseguo ls ")                     
        root.entries.foreach{a => a.print()}                                                       
    }                                             
}                                                                                                                                                          
                                                                                                                                                                                             
object terminale {                                                                                                                                                                            
    def main (args: Array[String]): Unit ={                                                                                                                                                   
        args.foreach{file =>                                                                                                                                                                  
            var testo = io.Source.fromFile(file).mkString                                                                                                                                     
            println("testo trovato : " + testo)                                                                                                                                               
            TParser.parseAll(TParser.parse, testo) match{                                                                                                                                     
                case TParser.Success(risultato, _) =>                                                                                                                                         
                    println("risultato parsato con successo: " + risultato)                                                                                                                   
                    val fs = new FileSystem                                                                                                                                                   
                    TerminalInterpreter.execute(risultato, fs)                                                                                                                                
                    println("eseguito tutto con successo !")                                                                                                                                                                                              
                                                                                                                                                                                              
                case TParser.NoSuccess(msg, _) => println("FALLIMENTO: "+ msg)                                                                                                                
            }                                                                                                                                                                                 
        }                                                                                                                                                                                     
    }                                                                                                                                                                                         
}                                                                                                                                                                                             