// definire una funzione is_palindrome che presa una stringa ritorna true se la stringa è palindroma
// togliendo tutti i caratteri particolari
                 
// definizione di un oggetto che contenga tutte le funzioni dell'esercizio
class Funzioni_esercizio{
  val is_palindrome = (str : String) =>{ 
    filtrata(str)//filtro della string
    .reverse.equals(filtrata(str))
  }              
                 
  //funzione che ritorna true se la stringa è un anagramma di una lista di stringhe
  val is_an_anagram = (str1 : String, str2 : String) =>{
    //strategia: confrontare le due stringhe ordinate
    str1.sorted == str2.sorted
  }              
                 
  //funzione che ritorna se il numero dato in ingresso è perfetto o no
  val is_proper = (n : Int) =>{
    
  }
  
  //funzione che presa una stringa ritorna la stessa ma filtrata dai caratteri speciali e lowerata
  val filtrata = (str : String) =>{
    str.filter(_.isLetterOrDigit).toLowerCase 
  }
}

object main extends App {
  println("Hello, world!")
  val C = new Funzioni_esercizio //creazione dell0'oggetto da cui richiamare le funzioni
  val str = "RiSe to vote sir"
  println("la stringa"+ str +" è palindroma? " + C.is_palindrome(str)) 
  
  println("la prima stringa è un anagramma della seconda? " + C.is_an_anagram("hello world", "helloworld "))
  println("finito l'esecuzione")
  
}

