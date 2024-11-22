(* Exercice 3 *)
type famille = {nom : string ; agesParents : int list ; agesEnfants : int list} ;;
type foyer = Celib of string*int | Famille of famille ;;

let enterprise = [Famille {Nom = "Dupont" ; AgesParents = [22 ; 25] ; AgesEnfants = [3]} ;
Celib ("Durand",18) ; Famille {Nom="Vador" ;AgesParents=[54] ;AgesEnfants=[26]} ;
Famille {Nom = "Martin" ; AgesParents = [32 ; 30] ; AgesEnfants = [2 ; 4 ; 6]}] ;;

(* 1- *)
let rec long = function 
  e::liste -> 1 + long(liste) |
  [] -> 0;; 

long([1;2;3;4]);;
- int = 4

(* 2- *)
let nbIndiv = function 
  Celib _ -> 1 |
  Famille({ agesParents = liste1; agesEnfants = liste2}) -> long(liste1) + long(liste2);;

