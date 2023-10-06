import scala.math                                                                                                                               
import scala.io.StdIn                                                                                                                           
                                                                                                                                                
//creazione delle classi necessarie per le operazioni e espressioni                                                                             
sealed trait Espressione                                                                                                                        
  case class Num (value: Int) extends Espressione                                                                                               
  case class Somma (primo: Espressione, secondo: Espressione) extends Espressione                                                                                    
  case class Sottrazione(primo: Espressione, secondo: Espressione) extends Espressione                                                                               
  case class Divisione (primo: Espressione, secondo: Espressione) extends Espressione                                                                                
  case class Moltiplicazione(primo: Espressione, secondo: Espressione) extends Espressione                                                                           
                                                                                                                                                 
// creazione del parser                                                                                                                                                                                                    
object Parser {                                                                                                                                                                                                            
  //operazione che dalla stringa ne estrae una lista di espressioni                                                                                                                                                        
    def parse (s : List[String]): (Espressione, List[String]) = s match {                                                                                                                                                  
                                                                                                                                                                                                                           
        //caso in cui inizia una nuova espressione                                                                                                                                                                         
        case "(" :: t => val (risultato, resto) = parse(t) //chiamo il parser sul resto 
        print("trovata una parentesi aperta, risultato: "+ risultato +" resto: "+resto)                                                                                                                                   
                        resto match{ // controllo se la espressione si conclude con una parentesi                                                                                                                          
                          case ")" :: r => (risultato, r)                                                                                                                                                                  
                          case _ => throw new IllegalArgumentException("le parentesi non risultano bilenciate")                                                                                                            
                        }                                                                                                                                                                                                  
        //mi trovo davanti ad un numero                                                                                                                                                                                                   
        case numero :: t =>  //ipotizzando che sia un numero e non una stringa tipo sqrt                                                                                                                                                  
             // allora mi trovo davanti ad una sottoespressione                                                                                                                                                                           
            val n: Num = new Num(numero.toInt)                                                                                                                                                                                            
            sottoespressione(n, t)                                                                                                                                                                                                        
                                                                                                                                                                                                                                          
        case Nil => throw new IllegalArgumentException("l'espressione non è valida")                                                                                                                                                      
    }                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                          
    def sottoespressione (esp : Espressione, resto: List[String]) : (Espressione, List[String]) = resto match {                                                                                                                           
        case Nil => (esp, Nil)                                          
        case "+" :: resto => val (destra, r) = parse(resto)                                                                                                                                                                               
                             println("parsato " + esp + " + " + destra)                                                                                                                                                                   
                             sottoespressione(Somma(esp, destra), r)                                                                                                                                                                      
        case "-" :: resto => val (destra, r) = parse(resto)                                                                                                                                                                               
                             sottoespressione(Sottrazione(esp, destra), r)                                                                                                                                                                
        case "/" :: resto => val (destra, r) = parse(resto)                                                                                                                                                                               
                             sottoespressione(Divisione(esp, destra), r)                                                                                                                                                                  
        case "*" :: resto => val (destra, r) = parse(resto)                                                                                                                                                                               
                             sottoespressione(Moltiplicazione(esp, destra), r)                                                                                                                                                            
        case ")" :: resto => (esp, List(")"))                                                          
        case _ => throw new IllegalArgumentException("argomento della sottoespressione non valido!")                                                                                                                                          
    }                                                                                                                                                                                                                      
                                                                                                                                                 
    //funzione per trasformare la stringa in tokens                                                                                              
    def tokenize (s: String): List[String] = {                                                                                                   
      s.replace("(", " ( ")                            
    .replace(")", " ) ")                               
    .replace("+", " + ")                     
    .replace("-", " - ")                     
    .replace("*", " * ")                     
    .replace("/", " / ")                                                                                                                
      .split(" ")                                                                                                                                
      .filter(_.nonEmpty)                                                                                                                        
      .toList                                                                                                                                    
    }                                                                                                                                            
                                                                                                                             
    //funzione che le raggruppa tutte                                                                                        
    def parsatutto (s: String): Espressione = {                                                                              
      val (risultato, _) = parse(tokenize(s))                                                                            
      risultato                                                                                                             
    }                                                                                                                        
}                                                                                                                            
                                                                                                                             
object Evaluator {                                                                              
    def calcola (op: Espressione) : Int = op match {                                                
        case Num(value) => value.toInt                                                                            
        case Somma(a, b) =>                                                                     
                            println("calcolo " + a+ " di classe :"+ a.getClass)                
                            val leftValue = calcola(a)                                          
                            val rightValue = calcola(b)                                         
                            leftValue + rightValue                                                            
        case _ => throw new IllegalArgumentException("l'operazione non è stata riconosciuta dall'evaluator")       
    }                                                                                                                       
                                                                                                                            
    def risultato (operazioni: Espressione) :Int = calcola(operazioni)                                                                                
}                                                                                                                           
                                                                                                                            
object dsl extends App{                                                                                                      
    var scanner = StdIn.readLine()                                                                                           
    while (scanner != null){                                                                                                 
        println("letta la riga : "+ scanner)                                                                                 
        println("stringa tokenizzata" + Parser.tokenize(scanner))                                                          
                                                                              
        val parsato = Parser.parsatutto(scanner)                                                                                                                                                                                             
        println("risultato del parser: " + parsato)                                                                        
                                                                                                    
        println("risultato : "+ Evaluator.risultato(parsato))                                                                     
                                                                                                                           
        scanner = StdIn.readLine()                                                                                           
    }                                                                                                                       
}                                                                                                                           