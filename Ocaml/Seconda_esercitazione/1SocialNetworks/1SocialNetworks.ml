(* gli individui sono rappresentati dai nodi
possono essere connessi attraverso legami di *)
module Graph = struct
type 'a grafo = Graph of ('a list) * (('a * 'a) list)
let empty() = Graph([],[])
let is_empty = function
|Graph (nodes, _) -> (nodes = [])

let grafo = [a;b;c;d;e] [(a,b);(b,c);(c,a)]
end
;;