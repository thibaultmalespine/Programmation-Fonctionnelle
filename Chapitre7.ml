(* Exercice 1 *)
  (* 1- *)
let rec min = function 
  a::b::liste -> if b < a then min(b::liste) else min(a::liste) |
  a::[] -> a |
  [] -> failwith("la liste ne doit pas être vide !");;
  
min([5;7;8;2;9]);; (* 2 *)
min([1;7;8;2;9]);; (* 1 *)
min([1;7;8;2;-5]);; (* -5 *)


  (* 2- *)
let rec enleve = function
  x, a::liste -> if x = a then liste else a::enleve(x, liste) |
  x, [] -> [];;

enleve(7, [5;7;8;7;9]);; (* [5;8;7;9] *)
enleve(1, [1;7;8;2;9]);; (* [7;8;2;9] *)
enleve(9, [1;7;8;2;9]);; (* [1;7;8;2] *)


  (* 3- *)
let rec naif = function
  [] -> [] | 
  liste -> let mini = min(liste) 
    in mini::naif(enleve(mini, liste));;
 
naif([5;7;8;2;9]);; (* [2;5;7;8;9] *)
naif([1;7;8;2;-5]);; (* [-5;1;2;7;8] *)


(* Exercice 2 *)
  (* 1- *)
let rec insertion = function 
  x, a::liste -> if x < a then x::a::liste else a::insertion(x, liste) |
  x, [] -> x::[];;

insertion(5, [2;4;8;10]);; (* [2;4;5;8;10] *)
insertion(1, [2;4;8;10]);; (* [1;2;4;8;10] *)
insertion(11, [2;4;8;10]);; (* [2;4;8;10;11] *)

  (* 2- *)
let rec tri_insere = function 
  a::liste -> insertion(a, tri_insere(liste)) |
  [] -> [];;

tri_insere([5;7;8;2;9]);; 
(* [2;5;7;8;9] *)
tri_insere([1;7;8;2;-5]);; 
(* [-5;1;2;7;8] *)
    

(* Exercice 3 *)
  (* 1- *)
(*Quelle stratégie algorithmique est ici utilisée ?*) (* Diviser pour régner *)
let rec divise = function 
  a::b::liste -> let t1,t2 = divise(liste) in a::t1, b::t2 |
  a::liste -> let t1,t2 = divise(liste) in a::t1, [] |
  [] -> [], [];; 
  

divise([7;5;3;2;4;8]);; (*[7; 3; 4], [5; 2; 8]*)

  (* 2- *)
let rec fusion = function 
  a::liste1, b::liste2 -> if a < b 
                          then a::fusion(liste1, b::liste2)  
                          else b::fusion(a::liste1, liste2) |
  [], liste2 -> liste2 |
  liste1, [] -> liste1 ;;

fusion([1;2;3],[4;5;6]);; (* [1;2;3;4;5;6] *)
fusion([1;3;5],[2;4;6]);; (* [1;2;3;4;5;6] *)
fusion([4;5;6],[1;2;3]);; (* [1;2;3;4;5;6] *)

  (* 3- *)
let rec tri_fusion = function
  [] -> failwith("la liste ne doit pas être vide !") |
  a::[] -> a::[] |
  liste -> let t1, t2 = divise(liste) in fusion(tri_fusion(t1), tri_fusion(t2));;

tri_fusion([5;7;8;2;9]);; (* [2;5;7;8;9] *)
tri_fusion([1;7;8;2;-5]);; (* [-5;1;2;7;8] *)


(* Exercice 4 *)
  (* 1- *)
let rec parcours = function 
  a::b::liste, inversion -> if b < a then 
    let t1, t2 = parcours(a::liste, inversion) in b::t1, true
  else 
    let t1, t2 = parcours(b::liste, inversion) in a::t1, t2 || false |
  a::[], inversion -> a::[], inversion |
  [], inversion -> failwith("la liste ne doit pas être vide !") ;;

  parcours([1;2;3;4;5;6;7], false);; (* [1;2;3;4;5;6;7], false *)
  parcours([7;2;3;4;5;6;1], false);; (* [2;3;4;5;6;1;7], true *)
  parcours([2;3;4;5;6;1;7], false);; (* [2;3;4;5;1;6;7], true *)
 
  (* 2- *)
let rec bulle_aux = function liste -> let t1, t2 = parcours(liste, false) in 
  if t2 
  then bulle_aux(t1) 
  else t1;;

  bulle_aux([7;2;3;4;5;6;1]);; (* [1;2;3;4;5;6;7] *)

  (* 3- *)
let tri_bulle = function liste -> bulle_aux(liste);;

  tri_bulle([7;2;3;4;5;6;1]);; (* [1;2;3;4;5;6;7] *)


(* Exercice 5 *)
  (* 1- *)
let rec partition = function
  a::liste, pivot -> if a > pivot 
    then let l1, l2 = partition(liste, pivot) in l1, a::l2
    else let l1, l2 = partition(liste, pivot) in a::l1, l2 |
  [], pivot -> [], [];; 

  partition([1;2;3;4;5;6], 2);; (* [1;2], [3;4;5;6] *)
  partition([1;3;5;7], 8);; (* [1;3;5;7], [] *)
  partition([5;7;3], 1);; (* [], [5;7;3] *)

  (* 2- *)
let rec quick = function 
  a::b::liste -> let l1, l2 = partition(b::liste, a) in quick(l1) @ a::quick(l2) |
  a::[] -> a::[] |
  [] -> [];;

  quick([2;1;5;3;6;4;4]);; (* [1;2;3;4;4;5;6] *)


(* Exercice 6 *)

let annuaire = [("Claude", 1785) ; ("Andrée", 6949) ;("Antoine", 1386) ;
("Françoise", 2638) ; ("Pascal",1009) ; ("Jean",2066) ; ("Ginette", 5250) ;
("Julien", 8043) ;("Pierre",4773) ; ("Paul",3367) ; ("Cécile",5843)] ;;

(* Recherche dans une liste quelconque *)
(* 1- *)
let rec cherche = function
  nom_cherche, element::liste -> let nom, numero = element in  
    if nom = nom_cherche then element else cherche(nom_cherche, liste) |
  nom_cherche, [] -> failwith("Cette personne n'est pas dans l'annuaire");;
(* TEST *)
cherche("Pierre", annuaire);; 
(* - : string * int = "Pierre", 4773 *)
cherche("Brice", annuaire) ;;
(* Exception : (Failure "Cette personne n'est pas dans l'annuaire") *)

(* 2- *)
let rec supprime = function
  nom_a_supprimer, element::liste -> let nom, numero = element in
    if nom_a_supprimer = nom then liste
    else element::supprime(nom_a_supprimer, liste) |
  nom_a_supprimer, [] -> [];;

supprime("Cécile", annuaire);;

(* Tri d’une liste de couples *)
(* 1- *)
let rec insere = function
  element_a_inserer, element::liste -> let nom, numero = element in let nom_a_inserer, numero_a_inserer = element_a_inserer in
    if nom_a_inserer < nom then element_a_inserer::element::liste
    else element::insere(element_a_inserer, liste) |
  element_a_inserer, [] -> element_a_inserer::[];;

  insere(("Thibault", 5844), annuaire);;

(* 2- *)
let rec tri_insertion = function
  element::liste -> insere(element, tri_insert(liste)) |
  [] -> [];; 

(* TEST *)
tri_insertion (annuaire) ;;
(* - : (string * int) list = ["Andrée", 6949 ; "Antoine", 1386 ;
"Claude",1785 ; "Cécile", 5843 ; "Françoise",2638 ; "Ginette", 5250 ;
"Jean", 2066 ; "Julien", 8043 ; "Pascal", 1009 ; "Paul", 3367 ;
"Pierre", 4773] *)


(* Utilisation de tables de hachage *)
(* 1- *)
let rec long = function
"" -> 0 |
mot -> 1+long(reste(mot)) ;;

long("bonjour");; (*-: int = 7 *)

(* 2- *)
let cle = function mot -> long(mot) mod 4;;

cle("bonjour");; (*-: int = 3 *)

(* 3- *)
let rec ajoutn = function
  0, element , liste::table_de_hachage -> let nouvelle_liste = element::liste in nouvelle_liste::table_de_hachage |
  cle, element, liste::table_de_hachage -> liste::ajoutn(cle - 1, element , table_de_hachage) |
  cle, element, [] -> [element]::[];;
  
ajoutn(cle("bonjour"), "bonjour", [["aaaa";"bbbb"]; ["salut"]]);; (* [["aaaa","bbbb"], ["salut"], ["bonjour"]] *)

(* 4- *)
let ajoute = function element, table_de_hachage -> let nom, numero = element in ajoutn(cle(nom), element, table_de_hachage);;

let init=[[] ;[] ;[] ;[]] ;;
ajoute(("Pierre",6547),init) ;;
(*- : (string * int) list list = [[] ; [] ; ["Pierre", 6547] ; []]*)

(* 5- *)
let rec hachage = function
  element::liste -> ajoute(element, hachage(liste)) |
  [] -> [[];[];[];[]];;

let table=hachage(annuaire) ;;
(* - : (string * int) list list = 
  [["Jean", 2066 ; "Paul", 3367] ;
  ["Françoise", 2638] ; 
  ["Claude", 1785 ; "Andrée", 6949 ; "Pascal", 1009 ; "Julien", 8043 ; "Pierre", 4773 ; "Cécile", 5843] ;
  ["Antoine", 1386 ; "Ginette", 5250]] *)

  (* 6- *)
let rec estPresent = function 
  nom_a_chercher, element::liste -> let nom, numero = element in 
    nom = nom_a_chercher || estPresent(nom_a_chercher, liste) |
  nom_a_chercher, [] -> false ;;

let rec rechercheListe = function
  0, nom_a_chercher, liste::table_de_hachage -> liste |
  cle, nom_a_chercher, liste::table_de_hachage -> rechercheListe(cle - 1, nom_a_chercher, table_de_hachage) |
  cle, nom_a_chercher, [] -> failwith("une erreur est survenue");;
 
let rechercheTable = function nom_a_chercher, table_de_hachage -> estPresent(nom_a_chercher, rechercheListe(cle(nom_a_chercher), nom_a_chercher, table_de_hachage));;

rechercheTable("Andrée", table);; (* true *)
rechercheTable("Thibault", table);; (* false *)

(* 7- *)
let rec trouverNumero = function 
  nom_a_chercher, element::liste -> let nom, numero = element in 
    if nom = nom_a_chercher then numero
    else trouverNumero(nom_a_chercher, liste) |
  nom_a_chercher, [] -> failwith("une erreur est survenue") ;;

let rechercheValeur = function nom_a_chercher, table_de_hachage -> trouverNumero(nom_a_chercher, table_de_hachage)