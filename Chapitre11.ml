(* LES PILES *)
type pile = PileVide | Empile of int * pile ;;

(* 1- *)

exception PileVideErreur;;

(* 2- *)

let p1 = Empile(1, Empile(2, Empile(3, PileVide)));;

(* 3- *)
let estVide = function 
  PileVide -> true |
  Empile _ -> false;;

estVide p1;;

(* 4- *)
let sommet = function 
  Empile (valeur, p) -> valeur |
  PileVide -> raise PileVideErreur;;
sommet p1 ;;

(* 5- *)
let empile = function n -> function p -> Empile(n,p);;
  
let p2 = empile 0 p1;;

(* 6- *)
let depile = function 
  Empile (n,p) -> p |
  PileVide -> raise PileVideErreur;;

let p3 = depile p2 ;;

(* 7- *)
let egalite = function p1 -> function p2 -> p1 = p2;;

egalite p1 p3;;
egalite p1 p2;;



(* EXPRESSION BIEN PARENTHESEE *)

(* 1- *)

let rec auxParentheses expr p = 
  match expr with
  "" -> estVide(p) |
  expr when tetec(expr) = '(' -> auxParentheses (reste(expr)) (empile 0 p) |
  expr when tetec(expr) = ')' -> auxParentheses (reste(expr)) (depile p) |
  expr -> auxParentheses (reste(expr)) p ;;

(* 2- *)

let parenthese = function expr -> try auxParentheses expr PileVide with PileVideErreur -> false;;

parenthese "(a+b" ;;
parenthese "(a+b)" ;;
parenthese "(a x (b + c))" ;;
parenthese "(a+b))" ;;
parenthese ")a+b(" ;;


(* LES FILES *)
type file = {debut : int list ; fin : int list} ;;

let f1 = {debut = [1 ;3] ; fin = [2 ;7 ;4]} ;;
let f2 = {debut = [] ; fin = [3 ;2 ;1]} ;;

(* 1- *)
exception FileVideErreur ;;

(* 2- *)
let estVide = function f -> f.debut = [] && f.fin = [];;

(* 3- *)
let rec transvase = function
  { debut = listeDebut ; fin = e::listeFin } -> transvase({ debut = e::listeDebut; fin = listeFin }) |
  { debut = e::listeDebut ; fin = [] } as f -> f;;

let f3 = transvase f2 ; ;
(*f3 : file = {debut = [1 ; 2 ; 3] ; fin = []}*)
  

(* 4- *)
let rec premier f =
  match f.debut, f.fin with
  e::listeDebut , listeFin -> e |
  [] , e::listeFin -> premier(transvase(f)) |
  [] , [] -> raise FileVideErreur;;

premier f1 ;;
(*- : int = 1*)
premier f2 ;;
(*- : int = 1*)

(* 5- *)
let enfiler e { debut = listeDebut; fin = listeFin } = { debut = listeDebut; fin = e::listeFin};;

let f5 = enfiler 0 f2 ;;
(*f5 : file = {debut = [] ; fin = [0 ; 3 ; 2 ; 1]}*)

(* 6- *)
let rec supprimerDernierElementListe = function 
  e1::e2::liste -> e1::supprimerDernierElementListe(e2::liste) |
  [e] -> [];;

let queue f =
  match f.debut, f.fin with  
  e::listeDebut, listeFin -> { debut = listeDebut; fin = listeFin } |
  [], e::listeFin -> { debut = []; fin = supprimerDernierElementListe(e::listeFin) } |
  [], [] -> raise FileVideErreur;;

queue f1;;
queue f2;;
queue {debut = []; fin = []};;

(* 7- *)
let egale f1 f2 = (transvase(f1)).debut = (transvase(f2)).debut ;;

egale f2 f3 ;;
egale f1 f3 ;;




(* TAS *)

type tas = Feuille of int | Noeud of int * tas * tas ;;

let tas1 = Noeud(18, Feuille(9), Feuille(7));;
let tas2 = Noeud(26, Feuille(11), Noeud(7, Feuille(5), Feuille(3)));;
let pastas = Noeud(15, Feuille(18), Feuille(9));;

(* 1- *)
let valMax = function 
  Noeud (n, t1, t2) -> n |
  Feuille n -> n;; 

valMax tas1 ;;
  - : int = 18
valMax tas2 ;;
  - : int = 26
  
(* 2- *)
let rec verif = function
  Noeud (n, t1, t2) -> n > valMax(t1) && n > valMax(t2) && verif(t1) && verif(t2) |
  Feuille n -> true ;;

verif tas1 ;;
- : bool = true
verif pastas ;;
- : bool = false


(* Multiensemble *)

let m1 = function
| 'a' -> 2
| 'b' -> 1
| 'c' -> 3
| _ -> 0 ;;

(* 1- *)

let combien m c = m(c);; 
combien m1 'a' ;;

(* 2- *)
let ajoute c m1 = function  
    charac when charac = c -> m1(c) + 1 |
    charac -> m1(charac) ;;

let m2 = ajoute 'b' m1 ;;
combien m2 'a' ;;
- : int = 2
combien m2 'b' ;;
- : int = 2
combien m2 'c' ;;
- : int = 3

(* 3- *)
let max n1 n2 = if n1 > n2 then n1 else n2 ;; 

let union m1 m2 = function c -> max (m1(c)) (m2(c)) + 0;;

let m3 = function 
'a' -> 1 |
'b' -> 2 |
_ -> 0;;

let m4 = union m1 m3;;
combien m4 'a';;
- : int = 2
combien m4 'b';;
- : int = 2
combien m4 'c';;
- : int = 3

(* Les tableaux dynamiques d’entiers *)

exception OutOfRange ;;

let v1 = function
0->7|
1->4|
2->3|
3->2|
_-> raise OutOfRange ;;

(* 1- *)
let affecte = function tab -> function (i,j) -> let newTab = function 
  x when x = i -> j |  
  x -> tab(x) 
in newTab;;

let v2 = affecte (v1)(1,5);; 
(v1 1, v2 1) ;;
(v1 3, v2 3) ;;


(* 2- *)
let supprime tab i = let newTab = function
  x when x >= i -> tab(x+1) |
  x -> tab(x) 
in newTab;; 


let v3 = supprime v1 1 ; ;
(v1 0, v3 0) ; ;
(v1 1, v3 1) ; ;
(v1 2, v3 2) ; ;
(v1 3, v3 3) ; ;


(* 3- *)
let insere tab (i,j) = let newTab = function
  x when x = i -> j |
  x when x > i -> tab(x-1) |
  x -> tab(x) 
in newTab;; 

let v4 = insere v3 (1,4) ; ;
(v1 0, v4 0) ; ;
(v1 1, v4 1) ; ;
(v1 2, v4 2) ; ;
(v1 3, v4 3) ; ;


(* Amélioration *)