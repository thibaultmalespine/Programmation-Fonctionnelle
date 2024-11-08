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
  Oper(c, e1, e2) -> match e1, e2 with
    Oper _ , Oper _ -> calcul(c, evalue(e1), evalue(e2)) |
    Oper _ , Nb n -> calcul(c, evalue(e1), n) |
    Nb n , Oper _ -> calcul(c, n, evalue(e2)) |
    Nb n1 , Nb n2 -> calcul(c, n1, n2);;
