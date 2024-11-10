(* Exercice 1 *)
  (* 1- *)
type couleur = Blanc | Noir| Rouge| Vert| Bleu |Jaune |Cyan |Magenta ;;


  (* 2- *)
let est_colore = function couleur -> not (couleur = Noir || couleur = Blanc) ;;

est_colore(Blanc);;
(* false *)
est_colore(Bleu);;
(* true *)

  (* 3- *)
let complementaire = function 
  Rouge -> Cyan |
  Magenta -> Vert |
  Jaune -> Bleu |
  Cyan -> Rouge |
  Vert -> Magenta |
  Bleu -> Jaune;;


(* Exercice 2 *)
type solution = D_nul of float | D_plus of float * float | D_moins of (float * float) * (float * float);;

D_moins ((0., 4), (4., 0));;


type polynome =  List_coef of int * float list | List_couples of (int * float) list;;

List_coef (3, [4.; 1.; 1.]);;
List_couples ([2, 1.; 0, 4.5]);;


(* Exercice 3 *)
  (* 1- *)
type nombreNR = N of int | R of float ;;

let test1 = N 5 ;;
let test2 = R 3.2 ;;


let somme = function 
 (N a, N b) -> N( a+b) 
 | (R a , R b) -> R( a +. b)
  | (N a, R b) -> R (float_of_int(a) +. b) 
  | (R b, N a) -> R (float_of_int(a) +. b) ;;

somme(N 5, N 4);;
somme(R 5.2, R 4.3);;
somme(R 5.5, N 4);;
somme(N 5, R 4.4);;

let produit = function
(N a, N b) -> N( a * b) 
| (R a , R b) -> R( a *. b)
  | (N a, R b) -> R (float_of_int(a) *. b) 
  | (R b, N a) -> R (float_of_int(a) *. b) ;;

produit(N 5, N 4);;
produit(R 5.2, R 4.3);;
produit(R 5.5, N 4);;
produit(N 5, R 4.4);;
  (* 2- *)
type nombreRC = R of float | C of float * float;;

  (* 3- *)


(* Exercice 4 *)
type exprLogique = Vrai | Faux | Non of exprLogique 
| Et of exprLogique*exprLogique
| Ou of exprLogique*exprLogique ;;

  (* 1- *)
let exp = Et ((Et(Vrai, Vrai),(Ou((Non(Ou((Faux, Non(Vrai)))))),Vrai)));;

  (* 2- *)
let rec echange = function 
  Vrai -> Faux |
  Faux -> Vrai |
  Non e -> Non (echange(e)) |
  Et (e1, e2) -> Et (echange(e1), echange(e2)) |
  Ou (e1, e2) -> Ou (echange(e1), echange(e2));;

  (* 3- *)
let rec evalue = function
  Vrai -> true |
  Faux -> false |
  Non e -> evalue(echange(e)) |
  Et (e1, e2) -> evalue(e1) && evalue(e2) |
  Ou (e1, e2) -> evalue(e1) || evalue(e2);;

  evalue(exp);;


(* Exercice 5 *)
type arbre = Feuille of int | Noeud of int*arbre*arbre ;;

let t= Noeud (4, Noeud (2, Noeud (3, Feuille 0, Feuille 1), Feuille 2),
Noeud (1, Feuille 2, Noeud (3, Feuille 1, Feuille 2))) ;;

  (* 1- *)
let rec profondeur = function 
  Noeud (n, succ1, succ2) -> let max = function a,b -> if a > b then a else b in 
  1 + max(profondeur(succ1), profondeur(succ2)) |
  Feuille n -> 0;;
  
profondeur(t);;

  (* 2- *)
let rec complet = function 
  n, 0 -> Feuille n |
  n, p -> Noeud(n, complet(n, p-1), complet(n, p-1));;

complet(7,2);;

  (* 3- *)
let rec liste = function 
  Noeud (n, succ1, succ2) -> n::liste(succ1)@liste(succ2) |
  Feuille n -> n::[];;

  liste(t);;


(* Exercice 6 *)

type expArithm = Nb of int | Oper of char*expArithm *expArithm ;;
(* 1- *)
let exp = Oper('+', Oper('*',Oper('+',Nb 0,Nb 1),Nb 2), Oper('/',Nb 2,Oper('-',Nb 1,Nb 2)));;
(* 2- *)
let rec nbOper = function 
  Oper (c,e1,e2) -> 1 + nbOper(e1) + nbOper(e2) |
  Nb n -> 0;;

nbOper(exp);;

(* 3- *)
let calcul = function (c, e1, e2) -> match c with
  '+' -> e1 + e2 |
  '-' -> e1 - e2 |
  '*' -> e1 * e2 |
  '/' -> e1 / e2 ;; 

  calcul('+',5,3);;

let rec evalue = function  
  Nb n -> n |
  Oper(c, e1, e2) -> calcul(c, evalue(e1), evalue(e2));;

evalue(exp);;


(* Exercice 7 *)

type objet = Chat | Clown | Mouton ;;

type mobile = Feuille of objet | Noeud of (int*int*mobile*mobile) ;;

let poids_f = function
Chat -> 1
| Clown -> 3
| Mouton -> 2 ;;

(* 1- *)
let fait_mob_simple = function n,  f -> Noeud(n, n, Feuille f, Feuille f);;

let m1 = fait_mob_simple(2, Chat);;

(* 2- *)
let rec poids_m = function 
  Noeud(poids1, poids2, mobile1, mobile2) -> poids_m(mobile1) + poids_m(mobile2) |
  Feuille objet -> poids_f(objet);;

  poids_m(fait_mob_simple(2,Feuille Clown));;

(* 3- *)
let rec agrandir = function 
  Noeud(poids1, poids2, mobile1, mobile2) -> Noeud( 2*poids1, 2*poids2, agrandir(mobile1), agrandir(mobile2)) |
  Feuille f -> Feuille f;;

agrandir(Noeud(1,2,Noeud(2,3, Feuille Chat, Feuille Clown), Feuille Mouton));;

(* 4- *)
let rec echanger_m = function 
  f1, f2, Noeud(poids1, poids2, mobile1, mobile2) -> Noeud(poids1, poids2, echanger_m(f1,f2,mobile1), echanger_m(f1,f2,mobile2)) |
  f1, f2, Feuille f -> if f = f1 then Feuille f2 else if f = f2 then Feuille f1 else Feuille f;; 

let m2 = Noeud(2,4, Feuille(Mouton), Noeud (3,4, Feuille (Chat), Feuille (Clown))) ;;

echanger_m (Chat, Mouton, m2) ;;

(* 5- *)
  (* a- *)
let rec tiges_m = function
  Noeud (l1, l2, mobile1, mobile2) -> l1 + l2 + tiges_m(mobile1) + tiges_m(mobile2) |
  Feuille f -> 0;;

tiges_m(m2);;

  (* b- *)
let rec lg_tiges_m = function
  Noeud (l1, l2, mobile1, mobile2) -> l1::l2::lg_tiges_m(mobile1) @ lg_tiges_m(mobile2) |
  Feuille _ -> [];;

lg_tiges_m(m2);;

  (* c- *)
let rec objets_m = function
  Noeud (l1, l2, mobile1, mobile2) -> objets_m(mobile1)@objets_m(mobile2) |
  Feuille f -> [f];;
  
objets_m(m1);;
objets_m(m2);;

(* 6- *)
  (* a- *)
let eq_loc = function 
  Noeud(l1, l2, m1, m2) -> l1*poids_m(m1) = l2*poids_m(m2) |
  Feuille _ -> true;;

  (* b- *)
let rec eq_glob = function
  Noeud (l1,l2,m1,m2) -> eq_loc(m1) && eq_loc(m2) && eq_glob(m1) && eq_glob(m2) |
  Feuille _ -> true;;

eq_glob(m1);;
eq_glob(m2);;

(* 7- *)
let max = function a, b -> if a > b then a else b;;

let rec profondeur = function
  Noeud (l1, l2, m1, m2) -> max(l1 + profondeur(m1), l2 + profondeur(m2)) |
  Feuille _ -> 0;;

profondeur(m2);;

(* 8- *)
let rec nombre_objets = function
  Noeud (l1, l2, m1, m2) -> 
    let chat1::clown1::mouton1::[] = nombre_objets(m1) in
    let chat2::clown2::mouton2::[] = nombre_objets(m2) in 
    chat1+chat2::clown1+clown2::mouton1+mouton2::[] |
  Feuille Chat -> [1;0;0] |
  Feuille Clown -> [0;1;0] |
  Feuille Mouton -> [0;0;1] ;;

nombre_objets(m1);;
nombre_objets(m2);;

(* 9- *)
let rec egale_m = function
    Noeud (l1,l2,m1,m2), Noeud(l3,l4,m3,m4) ->  l1 = l3 && l2 = l4 && egale_m(m1, m3) && egale_m(m2, m4) |
    Feuille f1, Feuille f2 -> f1 = f2 |
    _ -> false;;

    egale_m(m1, m2);;
    egale_m(m1, m1);;

(* 10- *)
let rec hauteur = function
    Noeud(_,_,m1,m2) -> hauteur(m1) + hauteur(m2) |
    Feuille _ -> 1;;

  hauteur(m2);;

(* 11- *)
let rec sous_mobile = function
    mobile1 , (Noeud(_,_,m1,m2) as mobile2) -> egale_m(mobile1, mobile2) || sous_mobile(mobile1,m1) || sous_mobile(mobile1,m2) |
    Feuille f1, Feuille f2 -> f1 = f2 |
    _ -> false;;

  sous_mobile( Noeud(3,4,Feuille Chat, Feuille Clown), m2);;
  sous_mobile( Noeud(3,4,Feuille Chat, Feuille Clown), m1);;
  sous_mobile( Feuille Chat, m1);;
  sous_mobile( Feuille Clown, m1);;

(* 12- *)
let rec remplacer = function 
    sous_mobile, mobile_de_remplacement, (Noeud(l1,l2,m1,m2) as mobile)  -> 
      if egale_m(sous_mobile, mobile) then mobile_de_remplacement 
      else Noeud (l1,l2,remplacer(sous_mobile, mobile_de_remplacement, m1), remplacer(sous_mobile, mobile_de_remplacement, m2))|
    sous_mobile, mobile_de_remplacement, Feuille f ->
      if sous_mobile = Feuille f then mobile_de_remplacement 
      else Feuille f;;

remplacer(m1, Feuille Chat, m1);;
let m3 = remplacer(Feuille Mouton, Noeud (3,4, Feuille (Chat), Feuille (Clown)) ,m2);;
remplacer(Noeud (3,4, Feuille (Chat), Feuille (Clown)), Feuille Chat, m3);;