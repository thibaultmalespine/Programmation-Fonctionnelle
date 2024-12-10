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
let rec transvase 
  { debut = d ; fin = f } -> { debut =  }