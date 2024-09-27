(* Exercice 1 *)

let rec somme = function
  1 -> 1 |  
  n -> n + somme(n-1);;


(* Exercice 2 *)

let rec puissance = function 
  (x, 1) -> x |
  (x, p) -> x * puissance(x, p-1);;
  