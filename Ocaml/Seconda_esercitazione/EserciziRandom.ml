(*programma che restituisce il fattoriale di quello che gli viene passato*)

let rec factorial (n: int): int =
  if n <= 0 then 1
  else n * factorial(n-1)
;;

let () =
  Printf.printf "%d\n" (factorial 4);
  for n = 0 to 16 do
    Printf.printf "%d! = %d\n" n (factorial n)
  done;
 

(*funzione che ritorna l'ultimo elemento di una lista*)
let rec last a = function
  | [] -> None
  | [ x ] -> Some x
  | _ :: t -> last t
;;

(*funzione che ritorna gli ultimi due elementi di una lista*)
let rec last2 a =
  match a with
  | [] -> None
  | [ x ] -> None
  | [ x ; y] -> Some(x,y)
  | _ :: t -> last t
;;

(*funzione che ritorna l'ennesimo elemento di una lista*)
let rec nth a i =
  match a with
  | [] -> None
  | h::t -> if i = 0 then Some h else nth t (i-1)
;;

(*funzione che ritorna la lunghezza di una lista*)
let length a = 
  let rec ausiliaria n = function
  | [] -> n
  | _ :: t -> ausiliaria (n+1) t
  in
  ausiliaria 0 a
;;

(*funzione che ritorna una lista ribaltata*)
let ribalta a =
  let rec ausiliaria nuova = function
    [] -> nuova
    | h :: t -> ausiliaria (h :: nuova) t
in ausiliaria [] a
;;

let ribaltaStringa s =
  let strlast = (String.length s)-1 in
  let rec aux original strlast nuova =
    match strlast with
    | -1 -> nuova 
    | _ -> nuova ^ String.make 1 original.[strlast] ^ aux original (strlast-1) nuova
  in aux s strlast ""
;;

(* funzione che capisca quando una stringa è palindroma *)

let palindroma s =
  if ribalta s = s then true else false 
;;

(* funzione flatten che data una lista di 'a * 'a list ne restituisce una lista equivalente *)

type 'a node = 
  | One of 'a
  | Many of 'a node list
;;

type 'a node = One of 'a | Many of 'a node list
;;

let flatten list =
  let aux finale = function
  |[] -> list
  |h :: t -> aux (list :: h) t
in aux [] list
;;

(* funzione che presa una lista ne elimina i duplicati *)

let rec compress = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | x -> x
;;

(* funzione che data una lista di elementi restituisca una 
   lista di liste che raggruppano gli elementi simili consecutivi *)

let pack list =
  let rec aux current acc = function
    | [] -> []    (* Can only be reached if original list is empty *)
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
       if a = b then aux (a :: current) acc t
       else aux [] ((a :: current) :: acc) t  in
  List.rev (aux [] [] list)
;;

(* funzione che ritorna il massimo comun denominatore *)

let rec mcd x y =
  let minore = if x > y then y else x in
  let maggiore = if x > y then x else y in
  (* funzione ricorsiva che divide il maggiore per il minore e nel caso diminuisce *)
  print_int (Int.rem maggiore minore);
  print_string "\n";
  let divisibili = if maggiore mod minore = 0 && minore mod maggiore = 0 then true else false in
  match divisibili with
  | true -> minore
  | false -> mcd (minore-1) maggiore
;;

(* funzione che stabilisce se una lista di interi contiene solo numeri pari
   e contiene un numero pari di elementi *)
let rec checkpari (list: int list) : bool =
 match list with
 | [] -> true
 | x::[] -> false
 |h :: h2 :: t -> h mod 2 = 0 && h2 mod 2 = 0 && checkpari t
;;

(* funzione isempty che ritorna true solo nel caso una lista è vuota *)
let isempty l =
  match l with
  | [] -> true
  | _ -> false
;;

(* funzione che ritorna il primo elemento di una lista, solleva una eccezione 
   se non ci sono elem *)

let primo list = 
  match list with
  |h :: t -> h
  |_ -> raise(Invalid_argument "porcatroia")
;;

(* funzione che somma tutti gli elementi in una lista di interi *)

let rec sommalista (interi : int list) : int =
  match interi with  
  | [] -> 0
  | h :: t -> sommalista t + h
;;

(* funzione che in una lista di interi trova quello più piccolo *)
let min a b : int = if a > b then b else a;;

let rec piccolo (interi : int list) : int =
  match interi with
  | [] -> raise (Invalid_argument "fail")
  | [x] -> x
  | h :: t -> min (piccolo t) h
;; 

(* funzione che appende una lista ad un altra *)

let rec appendi list1 list2 =
  match list1 with
  | [] -> list2
  | h :: t -> h :: (appendi t list2)
;;

(* funzione che determina se un elemento è presente in una lista *)

let rec presente (x : 'a) (list : 'a list) =
  match list with
  |[] -> false
  |h :: t -> if x = h then true else presente x t
;;

(* funzione che aggiunge "!" ad ogni stringa che gli viene passata come argomento *)

let esclama (s : string) : string =
  s ^ "!"
;;

(* funzione che prese in input una lista di stringhe restituisce una lista di tuple che siano 
composte come [stringa , lunghezza]*)

let type tupla = [string parola; int lunghezza];;

let rec ricorrenze (list: string list) : (string * int) list =
  match list with
  | [] -> []
  | h :: t -> (h, (String.length h)) :: ricorrenze t
;;

(* funzione che da una lista di coppie comed quella generata precedentemente
produce una coppia di liste *)

let rec primi (list: ('a * 'b) list) : 'a list =
  match list with
  | [] -> []
  | (a,b) :: t -> a :: primi t
;;

let rec secondi (list: ('a * 'b) list) : 'b list =
  match list with
  | [] -> []
  | (a,b) :: t -> b :: secondi t
;;

let coppia (list: (string * int) list) : (string list * int list) =
  match list with
  | [] -> ([],[])
  | (x,y) :: [] -> (x :: [], y :: [])
  | (x,y) :: (a,b) :: [] -> (x :: a :: [], y :: b :: [])
  | (x,y) :: t -> ( x :: (primi t), y :: (secondi t))
;;

(* data una lista di stringe metto il punto esclamativo a tutte *)

let esclama1 (s : string) = s^"!";;
let esclama2 (lista : string list) : (string list) =
  List.map esclama1 lista
;;

(* funzione che rende tutte le stringhe presenti in una lista in caps *)

let capitalize (lista : string list) : (string list) =
  List.map String.capitalize_ascii lista
;;

(*scrivere una funzione che eleva tutti gli argomenti di una lista al quadrato *)
let (-:) a = a ** 2.;;
let quadrati list =
  List.map (-:) list
;;

(* funzione che data una lista ritorni solo i suoi elementi dispari *)

let dispari list =
  List.filter (fun a -> (a mod 2 != 0)) list
;;

(* funzione che ritorna la lista di parole capitalized in una list *)

let capitalizzata (s : string) : bool =
  if String.get s 0 = Char.uppercase_ascii(String.get s 0) then true else false
;;
let filtrac list =
  List.filter capitalizzata list
;;

(* funzione che somma tutti gli interi in una lista *)

let somma (lista : int list) : int =
  List.fold_left (+) 0 lista
;;

(* funzione che data una lista ne ritorna la sua lunghezza *)

let rec lunghezza (lista : 'a list) : int =
  List.length lista
;;

(* funzione che determina se un numero è primo *)

let primo (n : int) : int =
  abs n
;;

let is_prime n =
  let n = abs n in
  let rec is_not_divisor d =
    d * d > n || 
  (n mod d <> 0 && is_not_divisor (d + 1)) in
  n <> 1 && is_not_divisor 2
;;

(* funzione che moltiplica tutti i valori in una lista di float *)

let moltiplica a b = a *. b

let moltilista (lista : float list) : float =
List.fold_left (moltiplica) 1. lista
;;

(* funzione che trova l'elemento più piccolo in una lista *)

let min a b = if a < b then a else b
let minimo (lista: int list) : int = 
  List.fold_left min max_int lista
;;

(* creazione di un albero binario *)

type 'a btree = Empty 
  | Node of 'a btree * 'a * 'a btree ;;

let btre1 : int btree =
  Node(Node(Empty, 2, Empty),
    3,
    Node(Empty,
      4,
      Node(Empty,5,Empty)
    )
  )
;;
(* funzione che somma tutti i valori in un albero*)

let rec sumnodes (albero : int btree) : int = 
  match albero with
  | Empty -> 0
  | Node(primo,secondo,terzo) -> secondo + sumnodes primo + sumnodes terzo

(* funzione che trasforma un albero binario in una lista *)

let rec tree2list (albero : int btree) : int list =
  match albero with
    | Empty -> []
    | Node(primo, secondo, terzo) -> (secondo :: []) @ tree2list primo @ tree2list terzo
;;

(* scrivere una funzione su tutti i nodi dellalbero *)

let rec tree_map (func : 'a -> 'b) (albero : 'a btree) : 'b btree =
 match albero with
 | Empty -> Empty
 | Node(f,s,t) -> Node(tree_map func f , func s , tree_map func t)
;;

let raddoppialbero (albero : 'a btree) : 'a btree =
  tree_map (fun a -> a * a) albero
;;

(* scrivere una funzione fold che funzioni sugli alberi *)

let rec fold_tree (e : 'b) (f : 'a -> 'b -> 'a -> 'a) (albero : 'a btree) : 'b =
  match albero with
  | Empty -> e
  | Node (first, second, third) -> f (fold_tree e f first) (second) (fold_tree e f third)
;;

(* calcolare una funzione che calcola il prodotto di un btree *)
let prodottree (albero : 'a btree) : int ->
  fold_tree 1 (fun first second third -> first * second * third) albero 
;;

let gray (int n) : string list =
  if n > 0 then
  else
;;

(* determino i fattori primi di un numero *)

let fattori (n : int): int list =
  let primlista = [2;3;5;7;11;13;17;19;21] in
in

let rec divisore (n : int) (lista : int list) : int =
  match lista with
  |[] -> n
  | h :: t ->if n mod h = 0 then h else divisore n t
in
  let rec confronta (n : int) : int list = 
    if n = 1 then [] 
    else divisore n primlista :: confronta (n / (divisore n primlista)) 
;;

(*funzione che prese due liste rende la seconda una copia della prima*)
let copia x y = 
match x with
| [] -> y
| h :: t -> List.nth 