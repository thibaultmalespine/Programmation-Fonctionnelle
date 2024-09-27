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

let operation = function
  (x,y,'+') -> x + y |
  (x,y,'-') -> x - y |
  (x,y,'*') -> x * y |
  (x,y,'/') -> if y <> 0 then x / y else failwith "La division par 0 est interdite !" |
  _ -> failwith "Arguments invalide" ;;


(* Exercice 5 *)

let prixTTC = function(ht, tva) -> (tva *. ht /. 100.) +. ht;;

let prix = function
  "pain" -> (1.05,5.5) |
  "conserve" -> (3.5,7.) |
  "disque" -> (12.3,18.6) |
  "bijou" -> (356.,33.) |
  _ -> failwith "Article indisponible" ;;

let plus_proche_entier = function x ->
  if x >= 0.0 
    then int_of_float(x +. 0.5) 
    else - int_of_float(abs_float(x) +. 0.5);; 

let arrondi = function x ->
float_of_int(plus_proche_entier(x *. 100.)) /. 100.;;
    

let sommeAPayer = function
  (nomArticle, nb) -> let prixTTC = prixTTC(prix(nomArticle))
    in 
  arrondi(float_of_int(nb) *. prixTTC) ;; 


(* Exercice 6 *)

let formule = function (j,m,p,s) -> j + (48 * m - 1)/5 + p/4 + p + s/4 - 2*s ;;

let decoupe = function n -> ((n/100), (n mod 100));;

let deuxMoisAvant = function
  (1, annee) -> (11, annee-1) |
  (2, annee) -> (12, annee-1) |
  (mois, annee) -> if mois <= 12 then (mois-2, annee) else failwith "Arguments invalides";; 

let leJour = function
    0 -> "Dimanche" |
    1 -> "Lundi" |
    2 -> "Mardi" |
    3 -> "Mercredi" |
    4 -> "Jeudi" |
    5 -> "Vendredi" |
    6 -> "Samedi" ;;

let modulo7 = function(entier) -> if entier >= 0 then entier mod 7 else (entier mod 7) + 7;;

let quelJour = function(jour, mois, annee) -> let (m,a) = deuxMoisAvant(mois, annee) 
  in 
    let (s,p) = decoupe(a)  
  in 
    let k = formule(jour, m, p, s)
  in
    leJour(modulo7(k));;