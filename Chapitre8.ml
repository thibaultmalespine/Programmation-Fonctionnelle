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
