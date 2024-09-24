(* Exercice 1 *)

let et = function 
  (true, true) -> true |
  _ -> false ;;

let ou = function
  (false, false) -> false |
  _ -> true ;;

let xor = function
  (true, false) -> true |
  (false, true) -> true |
  _ -> false ;;


(* Exercice 2 *)

let entier = function
  0 -> "zero" |
  1 -> "un" |
  n -> if (n mod 2) = 0 then "pair" else "impair" ;;


(* Exercice 3 *)

let point = function
  (0.,0.) -> "Origine" |
  (_,0.) -> "Axe des abscisses" |
  (0.,_) -> "Axe des ordonnées" |
  (x,_)-> if x > 0. then "point du demi plan x>0" else "point du demi plan x<0" ;; 


(* Exercice 4 *)

