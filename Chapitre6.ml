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
let ajoutDeuxFois = function element, liste -> element :: element :: liste;; 
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
let rec construitlistee = function 
  0 -> 0 :: [] |
  n -> n :: construitlistee(n-1);;

  construitlistee(9);;

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
  n, a::liste -> (n = a) || appartient(n, liste);; 

appartient(5,[1;2;3;4;5]);;
appartient(7,[1;2;3;4;5]);;


(* 2- *)
let rec maximum = function
  [] -> failwith("la liste ne doit pas être vide") |
  a :: [] -> a |
  a::b::liste -> if a >= b then maximum(a::liste) else maximum(b::liste);;

maximum([1;5;9;8;7;6;2]);;
maximum([1]);;
maximum([]);;


(* 3- *)
let rec occurrences = function
  n, [] -> 0 |
  n, a::[] -> if n=a then 1 else 0 |
  n, a::liste -> if n=a then 1 + occurrences(n, liste) else occurrences(n, liste);;
  
occurrences(4, [2;5;8;4;7;4;5]);;
occurrences(4, []);;


(* 4- *)
let rec fois2 = function
  [] -> [] |
  a::liste -> a*2 :: fois2(liste) ;;

fois2([1;2;3;4;5]);;


(* 5- *)
let rec insere = function
  n, [] -> n :: [] | 
  n, a::liste -> if a > n then n::a::liste else a :: insere(n, liste);;

insere(5,[1;2;3;4;6;7]);;



(* EXERCICE 4 *)
(* 1- *)
let rec ieme = function
  _, [] -> failwith("la liste est trop petite !") |
  1, a::liste -> a |
  n, a::liste -> ieme(n-1, liste);;

  ieme(5, [4;2;6;8;7;5]);;
  ieme(2, [4;2;6;8;7;5]);;


(* 2- *)
let rec prendre = function
  _, [] -> failwith("la liste est trop petite !") |
  1, a::liste -> a::[] |
  n, a::liste -> a::prendre(n-1, liste);;

  prendre(5, [4;2;6;8;7;5]);;
  prendre(2, [4;2;6;8;7;5]);;


(* 3- *)
let rec enleve = function
  _, [] -> failwith("la liste est trop petite !") |
  1, a::liste -> liste |
  n, a::liste -> enleve(n-1, liste);;

  enleve(5, [4;2;6;8;7;5]);;
  enleve(2, [4;2;6;8;7;5]);;


(* 4- *)
let rec melange = function
  [], [] -> [] |
  liste1, [] -> [] |
  [], liste2 -> [] |
  a::liste1, b::liste2 -> if a = b then a::melange(liste1, liste2) else melange(liste1, liste2);;

  melange([1;2;3;4], [1;2;3;4;5]);; (* [1;2;3;4] *)
  melange([1;2;3;4;5], [1;3;5]);; (* [1] *)
  melange([1;2;3;4;5], [0;2;5;4]);; (* [2;4] *)
  melange([1;3;5], [2;4]);; (* [] *)


  (* EXERCICE 5 *)

let fibonacci = function n -> let rec fibo = function
                                a, b, 1 -> a::[] |
                                a, b, n -> a::fibo(b,a+b,n-1)
  in fibo(1,1,n);;

fibonacci(8);;


(* EXERCICE 6 *)
(* 1- *)

let rec estCroissante = function 
  [] -> failwith("la liste ne peut pas être vide") |
  a::[] -> true |
  a::b::liste -> a <= b && estCroissante(b::liste);; 

  estCroissante([1;2;2;6;9]);;
  estCroissante([1;2;7;6;9]);;
  estCroissante([1;6;2;3]);;


(* 2- *)
let rec fusion = function 
  a::[], [] -> a::[] |
  [], b::[] -> b::[] |
  liste1, [] -> liste1 |
  [], liste2 -> liste2 |
  a::liste1, b::liste2 -> if a <= b then a::fusion(liste1, b::liste2) 
                          else b::fusion(a::liste1, liste2);;  

  fusion([2],[1;3]);;
  fusion([],[1;3]);;
  fusion([1;3],[2]);;
  fusion([2;4],[]);;
  fusion([2;4;8;12],[1;5;6;7;12]);;


(* EXERCICE 7 *)

(* 1- *)
let rec generer = function
  2 -> [2] |
  n -> generer(n-1)@[n];;

generer(7);;

(* 2- *)
let rec eliminer = function
  entier, [] -> failwith("la liste ne doit pas être vide !") |
  entier, a::[] -> if a mod entier = 0 then [] else a::[] |
  entier, a::liste -> if a mod entier = 0 then eliminer(entier, liste) 
                      else a::eliminer(entier, liste);; 

  eliminer(3, [3;1;2;6;4;12]);;
  eliminer(1, [3;1;2;6;4;12]);;

(* 3- *)
let eratos = function n -> let liste = generer(n) in
  let rec nombresPremier = function 
    num, [] -> failwith("fail") |
    num, a::[] -> a::[] |
    num, a::liste -> a::nombresPremier(num+1,eliminer(a, liste)) 
  in nombresPremier(2, liste);; 

  eratos(12);; (* [2;3;5;7;11] *)

(* 4- *)
let rec jumeaux = function 
  [] -> failwith("la liste ne doit pas être vide") |
  a::b[] -> if b-a = 2 then (a,b)::[] else [] |
  a::b::liste -> if b-a = 2 then (a,b)::jumeaux(b::liste) 
                 else jumeaux(b::liste);;