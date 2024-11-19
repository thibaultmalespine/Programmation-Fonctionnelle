(*Exercice 1*)
type point = {abs : float ; ord : float} ;;

let p1 = {abs = 0.0 ; ord = 0.0} and
p2 = {abs = 2.0 ; ord = 0.0} and
p3 = {abs = 1.0 ; ord = 2.0} and
p4 = {abs = 0.0 ; ord = 1.0} ;;

type forme = Cercle of point * float | Polygone of point list ;;

let p = Polygone [p1 ;p2 ;p3 ;p4 ;p1] ;;


(* 1- *)
let distance = function p1, p2 ->
  sqrt((p2.abs -. p1.abs)**2. +. (p2.ord -. p1.ord)**2.);;

distance(p3,p2);;
- : float = 2.2360679775

(* 2- *)
let rec longueur = function 
  a::b::liste -> distance(a,b) +. longueur(b::liste) |
  [a] -> 0.;;

longueur([p1;p2;p3]);;
- : float = 4.2360679775

(* 3- *)
let rec tailleListe = function
  a::liste -> 1 + tailleListe(liste) |
  [] -> 0;;

let rec premierEgalDernier = function
  a::b::liste when liste != [] -> premierEgalDernier(a::liste) |
  a::b::[] -> a.abs = b.abs && a.ord = b.ord;;

let bonPoly = function liste -> tailleListe(liste) > 3 && premierEgalDernier(liste);;

bonPoly ([p1 ;p2 ;p3 ;p4 ;p1]) ;;
- : bool = true
bonPoly ([p1 ;p2 ;p3 ;p4 ;p3]) ;;
- : bool = false
bonPoly ([p1 ;p2 ;p3]) ;;
- : bool = false


(* 4- *)
let perimetre = function 
  Polygone p -> if bonPoly(p) then longueur(p)
    else failwith("ce n'est pas un polygone") |
  Cercle (p, d) -> 2. *. 3.1415 *. d;;

perimetre p ;;
- : float = 6.65028153987
perimetre (Polygone [p1 ;p2 ;p3]) ;;
Exception : Failure "ce n'est pas un polygone"
perimetre (Cercle (p1,1.)) ;;
- : float = 6.28318


(*Exercice 2*)
(*Définition des types*)
(* 1- *)
type codeRVB = {r : int;  v : int; b : int};;
(* 2- *)
type couleur =  Rouge | Vert | Bleu | RVB of (int*int*int) | Melange of couleur * couleur ;;

(*Fonctions de conversion*)
(* 1- *)
let rvbToCoul = function code -> RVB (code.r, code.v, code.b);;

(* 2- *)
let rec coulToRvb = function
    Rouge -> {r = 255; v = 0; b = 0} |
    Vert -> {r = 0; v = 255; b = 0} |
    Bleu -> {r = 0; v = 0; b = 255} |
    RVB (r,v,b) -> {r = r; v = v; b = b} |
    Melange (c1, c2) -> let rvb1 = coulToRvb(c1) and rvb2 = coulToRvb(c2) in
    {r = ((rvb1.r + rvb2.r) / 2) ; v = ((rvb1.v + rvb2.v) /2) ; b = ((rvb1.b + rvb2.b) /2)};;

(* 3- *)
let tripletToCoul = function r,v,b -> {r = r; v = v; b = b};;

(* 4- *)
let coulToTriplet = function {r;v;b} -> r,v,b;;


(*Création de couleurs*)
(* 1- *)
let eclaircir = function code, param -> if code.r + param <= 255 && code.v + param <= 255 && code.b + param <= 255 then
  { r = code.r +param ; v = code.v +param;b = code.b +param} else failwith("Erreur");;

eclaircir({r = 52; v = 26; b = 75}, 50);;