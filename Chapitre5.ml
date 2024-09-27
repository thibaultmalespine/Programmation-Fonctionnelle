(* Exercice 1 *)

let rec somme = function
  0 -> 0 |  
  n -> n + somme(n-1);;


(* Exercice 2 *)

let rec puissance = function 
  (x, 0) -> 1 |
  (x, p) -> x * puissance(x, p-1);;
  

(* Bonus *)

let rec factorielle = function  
  0 -> 1 |
  x -> x * factorielle(x-1);;

(* Exercice 3 *)
let rec repet = function
  c,0 -> 0 |
  c,n -> c * puissance(10,n-1) + repet(c, n-1);;  

let rec unChiffre = function(n,c) ->
  if (n = c) then true else if (n < c) then false
  else (n mod 10) = c && unChiffre(n/10,c);;

let rec pgd = function
  n,1 -> 1 |
  n,d -> if (n mod d) = 0 then d else pgd(n, d-1);;
  
let nbPairChif = function(n) -> 
  let rec nbChif = function(n) ->  
    if n < 10 then 1
    else 1 + nbChif(n/10)
  in    
    (nbChif(n) mod 2) = 0;;
    