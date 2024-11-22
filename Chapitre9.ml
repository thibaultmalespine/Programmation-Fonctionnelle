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

coulToRvb(Melange(Rouge, Vert));;
- : codeRVB = {r = 127; v = 127; b = 0}

(* 3- *)
let tripletToCoul = function r,v,b -> {r = r; v = v; b = b};;

(* 4- *)
let coulToTriplet = function {r;v;b} -> r,v,b;;


(*Création de couleurs*)
(* 1- *)
let eclaircir = function code, param -> if code.r + param <= 255 && code.v + param <= 255 && code.b + param <= 255 then
  { r = code.r + param ; v = code.v + param;b = code.b + param} else failwith("Erreur");;

eclaircir({r = 52; v = 26; b = 75}, 50);;
- : codeRVB = {r = 102; v = 76; b = 125}

eclaircir({r = 52; v = 26; b = 75}, 181);;
Exception Failure "Erreur"

let assombrir = function code, param -> if code.r - param >= 0 && code.v - param >= 0 && code.b - param >= 0 then
  { r = code.r - param ; v = code.v - param;b = code.b - param} else failwith("Erreur");;

assombrir({r = 52; v = 76; b = 75}, 50);;
- : codeRVB = {r = 2; v = 26; b = 25}

assombrir({r = 52; v = 76; b = 75}, 181);;
Exception Failure "Erreur"

(* 2- *)
let plusClaire = function code -> eclaircir(code, 255 - maxRGB(code));;

plusClaire({r = 15; v = 85; b = 200});;
- : codeRVB = {r = 70; v = 140; b = 255}

let maxRGB = function 
  code when code.r >= code.v && code.r >= code.b -> code.r |
  code when code.v >= code.r && code.v >= code.b -> code.v |
  code when code.b >= code.v && code.b >= code.r -> code.b;;

maxRGB({r = 5;v = 15;b = 50});; 
- : int = 50

let plusSombre = function code -> assombrir(code, minRGB(code));;

plusSombre({r = 15; v = 85; b = 200});;
- : codeRVB = {r = 0; v = 70; b = 185}

let minRGB = function 
  code when code.r <= code.v && code.r <= code.b -> code.r |
  code when code.v <= code.r && code.v <= code.b -> code.v |
  code when code.b <= code.v && code.b <= code.r -> code.b;;

minRGB({r = 5;v = 15;b = 50});; 
- : int = 5


let nuancesRouges = function code -> nuancesRougesRec({r = 255; v = code.v; b = code.b});;

let rec nuancesRougesRec = function
  {r = 0; v = x2; b = x3} -> [{ r = 0; v = x2; b = x3 }] |
  {r = x1; v = x2; b = x3} -> { r = x1; v = x2; b = x3 }::nuancesRougesRec({ r = x1 - 1; v = x2; b = x3 });;

nuancesRouges({r = 88; v = 25; b = 152});;


(*Exercice 3*)
(* 1- *)
type monome = {coeff :float ; deg :int} ;;
type polynome = Plein of int* float list | Creux of monome list ;;
let p = Plein(4,[1. ;0. ;3. ;0. ;5.]) ;;

let ajout = function (monome, Creux(liste)) -> Creux(ajoutRec(monome, liste));;

let rec ajoutRec = function
monome, (m::liste) ->  if m.deg = monome.deg then 
  ( let coeff = m.coeff +. monome.coeff and deg = m.deg in 
  if coeff = 0. then liste  else {coeff; deg}::liste )
else m::ajoutRec(monome, liste) |
monome, ([]) -> [monome];; 

let c = Creux([{coeff = 2.; deg = 2};{coeff = 1.5; deg = 1}]);;
  
ajout({coeff = 1.; deg = 3}, c);;
- : polynome = Creux [{coeff = 2.; deg = 2}; {coeff = 1.5; deg = 1}; {coeff = 1.; deg = 3}]

ajout({coeff = -1.5; deg = 1}, c);;
- : polynome = Creux [{coeff = 2.; deg = 2}]

ajout({coeff = 2.; deg = 2}, c);;
- : polynome = Creux [{coeff = 4.; deg = 2}; {coeff = 1.5; deg = 1}]

(* 2- *)
let pleinVersCreux = function (Plein(n,liste)) -> Creux(pleinVersCreuxRec(n, liste));;

let rec pleinVersCreuxRec = function 
n, f::liste -> if f = 0. then pleinVersCreuxRec(n-1, liste) else {coeff = f; deg = n}::pleinVersCreuxRec(n-1, liste) |
_, [] -> [];;

pleinVersCreux(p);;
- : polynome = Creux [{coeff = 1.; deg = 4};{coeff = 3.; deg = 2};{coeff = 5.; deg = 0}] 