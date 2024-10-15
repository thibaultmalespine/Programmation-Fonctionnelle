(* EXERCICE 1 *)
(* 1- *)
let deuxieme = function
  a::b::q -> b |
  _ -> failwith("la liste doit avoir au moins deux éléments");;

deuxieme([1;2;3;4;5]);;
deuxieme([2;3;4;5;1]);;
deuxieme([]);;
deuxieme([2]);;
deuxieme([5;8]);;

(* 2- *)
let auMoinsTrois = function
  a::b::c::q -> true |
  _ -> false ;;

auMoinsTrois([1;2;3]);;
auMoinsTrois([1;2]);;

(* 3- *)
let sommeTrois = function
  a::b::c::q -> a+b+c |
_ -> failwith("la liste doit avoir au moins trois éléments") ;;

sommeTrois([1;2;3]);;
sommeTrois([1;2]);;

(* 4- *)
let troisEstPair = function 
  a::b::c::q -> c mod 2 == 0 |
  _ -> failwith("la liste doit avoir au moins trois éléments") ;;

  troisEstPair([1;2;3]);;
  troisEstPair([1;2;4]);;
  troisEstPair([1;2]);;

(* 5- *)
let ajoutDeuxFois = function element, list -> element :: element :: list;; 
ajoutDeuxFois(1,[]);;
ajoutDeuxFois(1,[3;4]);;

(* 6- *)
let permute = function
  a::b::q -> b::a::q |
  _ -> failwith("la liste doit avoir au moins deux éléments") ;;

permute([1;2;3]);;
permute([1;2]);;
permute([1]);;



(* EXERCICE 2 *)
(* 1- *)
let rec construitListe = function 
  0 -> 0 :: [] |
  n -> n :: construitListe(n-1);;

  construitListe(9);;

(* 2- *)
let rec longueur = function
 [] -> 0 |
 a::q -> 1 + longueur(q);;

 longueur([5;5;5;5;5]);;

(* 3- *)
let rec dernier = function
[] -> failwith("la liste ne doit pas être vide") |
a::[] -> a |
a::q -> dernier(q);; 

dernier([0;3;6;9]);;
dernier([1]);;
dernier([]);;

(* 4- *)
let rec somme = function
[] -> 0 |
a::q -> a + somme(q) ;;

somme([0;3;6;9]);;
somme([1]);;
somme([]);;

(* 5- *)
let rec taillePaire = function
[] -> true |
a::q -> not(taillePaire(q));;

taillePaire([1;2;3]);;
taillePaire([1;2]);;

(* 5-bis *)
let rec taillePaire = function
[] -> true |
a :: [] -> false |
a::b::q -> taillePaire(q);;

taillePaire([1;2;3]);;
taillePaire([1;2]);;


(* 6- *)
let rec rangImpair = function
[] -> [] |
a :: [] -> a :: [] |
a::b::q -> a::rangImpair(q);;

rangImpair([1;2;3;4;5;7;6]);;
rangImpair([1;2;3;4;5;6]);;



(* EXERCICE 3 *)
(* 1- *)

let rec appartient = function
  n, [] -> false |
  n, a::list -> (n = a) || appartient(n, list);; 

appartient(5,[1;2;3;4;5]);;
appartient(7,[1;2;3;4;5]);;


(* 2- *)
let rec maximum = function
  [] -> failwith("la liste ne doit pas être vide") |
  a :: [] -> a |
  a::b::list -> if a >= b then maximum(a::list) else maximum(b::list);;

maximum([1;5;9;8;7;6;2]);;
maximum([1]);;
maximum([]);;


(* 3- *)
let rec occurrences = function
  n, [] -> 0 |
  n, a::[] -> if n=a then 1 else 0 |
  n, a::list -> if n=a then 1 + occurrences(n, list) else occurrences(n, list);;
  
occurrences(4, [2;5;8;4;7;4;5]);;
occurrences(4, []);;


(* 4- *)
let rec fois2 = function
  [] -> [] |
  a::list -> a*2 :: fois2(list) ;;

fois2([1;2;3;4;5]);;


(* 5- *)
let rec insere = function
  n, [] -> n :: [] | 
  n, a::list -> if a > n then n::a::list else a :: insere(n, list);;

insere(5,[1;2;3;4;6;7]);;



(* EXERCICE 4 *)
(* 1- *)

