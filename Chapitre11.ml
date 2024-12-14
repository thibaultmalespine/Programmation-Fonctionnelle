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

type tabDyn = {taille : int; valeurs : int -> int};;

let d1 = {taille = 4; valeurs = v1};;

(* 1- *)
let affecteDyn tabD (i,j) = {taille = tabD.taille; valeurs = function n when n = i -> j | _ -> tabD.valeurs(i) };; 

7
let d2 = affecteDyn d1 (1,5) ;;
(* d2 : tabDyn = {taille = 4 ; valeurs = <fun>} *)

d2.valeurs 1 ;;
(* - : int = 5 *)


(* 2- *)
let supprime tabD i = { taille = tabD.taille -1 ; valeurs = function
| n when n >= i -> tabD.valeurs(n+1)
| n -> tabD.valeurs(n)};;

let d3 = supprime d1 1;;
d3.valeurs(1);;


(* 3- *)
let insereDyn tabD (i,j) = {taille = tabD.taille +1 ; valeurs = function
| n when n > i -> tabD.valeurs(n-1)
| n when n = i -> j
| n -> tabD.valeurs(n)};;

let d4 = insereDyn d3 (1,4) ;;
(*d4 : tabDyn = {taille = 4 ; valeurs = <fun>}*)
d4.valeurs(1);;

(* 4- *)
let egalite tabD1 tabD2 = let rec isEgal = function  
| 0 -> tabD1.valeurs(0) = tabD2.valeurs(0)
| n -> tabD1.valeurs(n) = tabD2.valeurs(n) && isEgal(n-1)
in tabD1.taille = tabD2.taille && isEgal (tabD1.taille -1) ;;

egalite d1 d4 ;;
(*- : bool = true*)

(* 5- *)

let listeTest = [0;1;2;3;4;5;6;7];;

let indice liste n = let rec findIndex = function
| a::liste, 0 -> a 
| a::liste, n -> findIndex(liste, n-1) 
in findIndex(liste, n);;  

indice listeTest 3;;
(* - int = 3 *)


(* 6- *)
let rec longueur = function 
  a::liste -> 1 + longueur(liste) |
  [] -> 0;;
  
longueur listeTest;;
(* - int = 8 *)


(* 7- *)
let listeVersTableau liste = { taille = longueur(liste); valeurs = function n -> indice(liste)(n) };;

(* 8- *)
let tableauVersListe tabD = let rec createList = function 
| indice when indice < tabD.taille -1 -> tabD.valeurs(indice)::createList(indice+1)
| indice -> [tabD.valeurs(indice)]
in createList(0);;

let listeTest = [1 ;4 ;7 ;8 ;10] ;;
tableauVersListe (listeVersTableau µlisteTest) ;;
(*listeTest : int list = [1 ; 4 ; 7 ; 8 ; 10]
- : int list = [1 ; 4 ; 7 ; 8 ; 10]*)


(* LES GRAPHES *)
type graphe = {sommets : int ; adjacences : int -> int list} ;;
(* 1- *)
exception PasUnSommet;;

(* 2- *)
let g1 = { sommets = 5; adjacences = function
| 1 -> [5]
| 2 -> [1;4]
| 3 -> [2]
| 4 -> [3]
| 5 -> [2;4]
| _ -> raise PasUnSommet
};;

(* 3- *)
let rec longueur = function 
  a::liste -> 1 + longueur(liste) |
  [] -> 0;;

longueur(g1.adjacences(2));;

(* 4- *)
let nombreVoisins g sommet = longueur(g.adjacences(sommet));;

(* 5- *)
let nombreArcs g = let rec nombreArcsRec = function
| 1 -> longueur(g.adjacences(1)) 
| n -> longueur(g.adjacences(n)) + nombreArcsRec(n -1)
in nombreArcsRec(g.sommets);;

nombreArcs g1 ;;
(* - : int = 7 *)


(* 6- *)

let initGraphe n = { sommets = n ; adjacences = function sommet -> if sommet <= n && sommet >= 1 then [] else raise PasUnSommet};;
let g = initGraphe 5;;
g.adjacences(5);;

(* 7- *)
let ajoutSommet g = {sommets = g.sommets +1; adjacences = function sommet ->
  if sommet <= g.sommets && sommet >= 1 then g.adjacences(sommet)
  else if sommet = g.sommets+1 then []
  else raise PasUnSommet };;

  let g1b = ajoutSommet g1 ;;
  (*g1b : graphe = {sommets = 6 ; adjacences = <fun>} *)
  g1b.adjacences 1 ;;
  (*- : int list = [5]*)
  g1b.adjacences 6 ;;
  (*- : int list = []*)


(* 8- *)

let rec isInListe = function
| e::liste, element -> if element != e then isInListe(liste, element) else true
| [], element -> false ;;

let rec ajoutListe element liste = if isInListe(liste, element) then liste else element::liste;;

let listeTest = [1;2;3;4];;
ajoutListe(3)(listeTest);; 
(* [1;2;3;4] *)
ajoutListe(5)(listeTest);; 
(* [1;2;3;4;5] *)


(* 9- *)
let ajoutArc g (x,y) = {sommets = g.sommets ; adjacences = function n -> if n = x then ajoutListe(y)(g.adjacences(x)) else g.adjacences(n)};;

let g1c = ajoutArc g1b (6,1);;
g1c.adjacences 6;;
g1c.adjacences 1;;

(* 10- *)
let arcToG n listeArc = let g = initGraphe n in let rec arcToGRec = function
| (x,y)::listeA, g -> arcToGRec(listeA, ajoutArc(g)(x,y))
| [], g -> g 
in arcToGRec(listeArc, g);;

let listeArc = [(1,5);(2,1);(2,4);(3,2);(4,3);(5,2);(5,4)];;

let g4 = arcToG 5 listeArc;;

g4.adjacences(1);;
g4.adjacences(2);;


(* PARCOURS EN PROFONDEUR SIMPLE *)

let rec appartient = function
| e::liste, element -> if element != e then isInListe(liste, element) else true
| [], element -> false ;;

let rec auxProfond g i listeVisite = let newListeVisite = listeVisite@[i] in let rec parcoursSuccesseurs = function 
| successeur::listeAdj, listeVisite -> if appartient(listeVisite, successeur) then parcoursSuccesseurs(listeAdj,listeVisite) else auxProfond(g)(successeur)(listeVisite)
| [], listeVisite -> listeVisite
in parcoursSuccesseurs(g.adjacences(i), newListeVisite);;

let profond g i = let visite = [] in auxProfond(g)(i)(visite) ;;

profond g1 1 ;;
(*- : int list = [1 ; 5 ; 2 ; 4 ; 3]*)
profond g1c 1 ;;
(*- : int list = [1 ; 5 ; 2 ; 4 ; 3]*)
profond g1c 6 ;;
(*- : int list = [6 ; 1 ; 5 ; 2 ; 4 ; 3]*)


(* Parcours en largeur - Version 1 *)

let largeur g i = 
  let rec ajout l1 f lv = 
    match l1 with
  | sommet::l1 -> if appartient(lv, sommet) then ajout(l1)(f)(lv) else ajout(l1)(f@[sommet])(lv)
  | [] -> f 
  in let rec auxLargeur lv f = 
    match f with 
    | sommet::f -> let newLv = if appartient(lv, sommet) then lv else lv@[sommet] in auxLargeur(newLv)(ajout(g.adjacences(sommet))(f)(newLv))
    | [] -> lv
  in auxLargeur([])([i]);;


largeur g1c 1 ;;
largeur g1c 6 ;;
(*- : int list = [1 ; 5 ; 2 ; 4 ; 3]
- : int list = [6 ; 1 ; 5 ; 2 ; 4 ; 3]*)


(* Parcours en largeur - Version 2 *)
(* 1- *)
let rec union l1 l2 = 
  match l1 with
  | sommet::l1 -> if appartient(l2,sommet) then union(l1)(l2) else sommet::union(l1)(l2) 
  | [] -> l2;;
(* 2- *)
let uneEtape g i = union([i])(g.adjacences(i));;
uneEtape g1 1;;

(* 3- *)
let rec listeEtape g liste = 
  match liste with
  | sommet::liste -> union(uneEtape(g)(sommet))(listeEtape(g)(liste))
  | [] -> [];;

  listeEtape g1 [2 ;5] ;;

(* 4- *)
let rec largeurAux g liste = 
  let nouvelleListe = listeEtape(g)(liste) in 
  if longueur(nouvelleListe) = longueur(liste) then liste else largeurAux(g)(nouvelleListe);;

(* 5- *)
let largeur2 g i = largeurAux(g)([i]);;

largeur2 g1 1;;