(* Partie I *)
(* 1- *)
let rec nbOcc = function 
  motif, a::liste when motif = a -> 1 + nbOcc(motif, liste) |
  motif, a::liste -> nbOcc(motif, liste) |
  motif, [] -> 0;;

  (* type : 'a * 'a list -> int = <fun> *)

  (* test *)
nbOcc('a', ['a';'n';'a';'c';'o';'n';'d';'a']);; (* int : 3 *)
nbOcc('a', ['n';'u';'l';'l']);; (* int : 0 *)


(* 2- *)
let rec repet = function
  0, a -> [] |
  n ,a -> a::repet(n-1, a);;

  (* type : int * 'a -> 'a list = <fun> *)

  (* test *)
repet(7,3) ;;
(* - : int list = [3 ; 3 ; 3 ; 3 ; 3 ; 3 ; 3] *)
repet(7,'a') ;;
(* - : char list = [`a` ; `a` ; `a` ; `a` ; `a` ; `a` ; `a`] *)


(* 3- *)
let rec nPremiers = function 
  0, liste -> [] |
  n, a::liste -> a::nPremiers(n-1, liste) |
  _ -> failwith("la liste est trop courte");;

(* type : int * 'a list -> 'a list = <fun> *)

  (* test *)
nPremiers(4,[2 ;3 ;4 ;5 ;6 ;7 ;8]) ;; 
(* - : int list = [2 ; 3 ; 4 ; 5] *)
nPremiers(4,[2 ;3 ;4]) ;;
(* Exception : Failure "la liste est trop courte" *)

(* 4- *)
let rec tranche = function
  1, m, liste -> nPremiers(m, liste) |
  n, m, a::liste -> tranche(n-1, m-1, liste) |
  _ -> failwith("la liste est trop courte");;
  (* type *)

  (* test *)
tranche(2,4,[1 ;2 ;3 ;4 ;5 ;6 ;7]) ;;
(* - : int list = [2 ; 3 ; 4] *)
tranche(2,6,[1 ;2 ;3 ;4 ;5 ;6 ;7]) ;;
(* - : int list = [2 ; 3 ; 4 ; 5 ; 6] *)
tranche(2,9,[1 ;2 ;3 ;4 ;5 ;6 ;7]) ;;
(* Exception : Failure "la liste est trop courte" *)


(* Partie II *)
(* 1- *)
type chifoumi = Pierre | Feuille | Ciseaux;;

let quiGagne = function 
  Pierre -> Feuille |
  Feuille -> Ciseaux |
  Ciseaux -> Pierre ;;

  (* type : quiGagne : chifoumi -> chifoumi = <fun> *)

(* test *)
quiGagne(Pierre) ;;
(*- : chifoumi = Feuille*)
quiGagne(Feuille) ;;
(*- : chifoumi = Ciseaux*)
quiGagne (Ciseaux) ;;
(*- : chifoumi = Pierre*)

(* 2- *)
let duel = function a, b -> if quiGagne(a) = b then b else if quiGagne(b) = a then a else failwith("Egalite !") ;;

  (* type : duel : chifoumi * chifoumi -> chifoumi = <fun> *)

duel (Feuille, Ciseaux) ;;
(*- : chifoumi = Ciseaux*)
duel (Feuille, Feuille) ;;
(*#Exception : Failure "Egalite !"*)


(* Partie III *)
type arbre_binaire = Vide | Noeud of int * arbre_binaire * arbre_binaire ;;

let abr1 = Noeud( 8,
Noeud (3, Noeud(2,Vide,Vide), Noeud(6,Vide, Vide)),
Noeud (19, Vide, Vide)) ;;

let abr2 = Noeud( 5,
Noeud (3, Noeud(2,Vide,Vide), Noeud(5,Vide, Vide)),
Noeud (7, Vide, Noeud(8,Vide,Vide))) ;;

(* 1- *)
let rec taille = function 
  Noeud (n, fils_gauche, fils_droit) -> 1 + taille(fils_gauche) + taille(fils_droit) |
  Vide -> 0;;

  (* type : arbre_binaire -> int = <fun> *)

(* test *)
taille abr1 ;;
(* - : int = 5 *)
taille abr2 ;;
(* - : bool = 6 *)

(* 2- *)
let rec recherche = function
  valeur, Noeud (n, fils_gauche, fils_droit) when valeur = n -> true |
  valeur, Noeud (n, fils_gauche, fils_droit) -> if valeur <= n then recherche(valeur, fils_gauche) else recherche(valeur, fils_droit) |
  valeur, Vide -> false ;; 

  (* type : int * arbre_binaire -> bool = <fun> *)

(* test *)
recherche (6, abr1) ;;
- : bool = true
recherche (12, abr2) ;;
- : bool = false

(* 3- *)
let rec insertion = function
  valeur, Noeud (n, fils_gauche, fils_droit) -> 
    if valeur <= n then Noeud (n, insertion(valeur, fils_gauche), fils_droit) 
    else Noeud (n, fils_gauche, insertion(valeur, fils_droit)) | 
  valeur, Vide -> Noeud(valeur, Vide, Vide);;

(* type : int * arbre_binaire -> arbre_binaire = <fun> *)

  (* test *)
insertion(4, abr1) ;;
(*- : arbre_binaire =
Noeud( 8,
Noeud (3, Noeud(2,Vide,Vide), Noeud(6,Noeud(4,Vide, Vide, Vide)),
Noeud (19, Vide, Vide)) *)
insertion(3, abr2) ;;
(* - : arbre_binaire =
Noeud( 5,
Noeud (3, Noeud(2,Vide,Noeud(3,Vide,Vide)), Noeud(5,Vide, Vide)),
Noeud (7, Vide, Noeud(8,Vide,Vide))) *)

let rec list_to_arbre = function
  a::b::liste -> insertion(a, list_to_arbre(b::liste))| 
  a::[] -> Noeud(a, Vide, Vide) |
  [] -> Vide;;

(* type : int list -> arbre_binaire = <fun> *)

  (* test *)
list_to_arbre [8 ;3 ;2 ;19 ;6] ;;
(*#- : arbre_binaire =
 Noeud
  (6, Noeud (2, Vide, Noeud (3, Vide, Vide)),
   Noeud (19, Noeud (8, Vide, Vide), Vide))*)

(* 5- *)
let rec parcours_infixe = function
Noeud(etiquette, fils_gauche, fils_droit) -> parcours_infixe(fils_gauche) @ [etiquette] @ parcours_infixe(fils_droit)| 
Vide -> [];;

(* type : arbre_binaire -> int list = <fun> *)

    (* test *)
parcours_infixe abr1 ;;
- : int list = [2 ; 3 ; 6 ; 8 ; 19]

(* 6- *)
let tri = function liste -> parcours_infixe(list_to_arbre(liste));;

(* int list -> int list = <fun> *)

  (* test *)
tri [8 ;3 ;2 ;19 ;6];;
- : int list = [2 ; 3 ; 6 ; 8 ; 19]


(* 7- *)

let a_doublons = function arbre -> recherche_doublons(tri(parcours_infixe(arbre)));;
(* type a_doublons : arbre_binaire -> bool = <fun> *)
let rec recherche_doublons = function 
    a::b::liste -> a = b || recherche_doublons(b::liste) |
    a::[] -> false |
    [] -> false;; 
(* type recherche_doublons : 'a list -> bool = <fun> *)

a_doublons abr1 ;;
- : bool = false
a_doublons abr2 ;;
- : bool = true


(* 8- *)

let est_trie = function arbre -> tri(parcours_infixe(arbre)) = parcours_infixe(arbre);;
(* est_trie : arbre_binaire -> bool = <fun> *)

est_trie abr1 ;;
- : bool = true