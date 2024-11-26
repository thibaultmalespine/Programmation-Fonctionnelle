(* Exercice 1 *)

(* 1- *)
let fzz = function f -> function a -> f(a+1) = 0 ;;
val fzz : ( int -> int ) -> int -> bool

(* 2- *)
fzz(3);;  "erreur de typage"

let g = fzz(function n -> 3);; 
val g : int -> bool 

g 4;; 
- : bool = false

(* 3- *)
let a = function n -> function p -> n+1 = p;;
let b = function n -> n(1) = 2;;
let c = function n -> function p -> if p && n > 0 then 7.5 else 5.;; 
let d = function n -> if n(5) then 5. else 0.;; 


(* Exercice 2 *)
let predicat = function n -> n > 10;; 

(* 1- *)
let rec ilexiste = function p -> function
  element::l -> p(element) || ilexiste(p)(l) |
  [] -> false;;

ilexiste(predicat)([1;2;3;4;5]);; "false"
ilexiste(predicat)([1;2;30;4;5]);; "true"

(* 2- *)
let rec qqsoit = function p -> function 
  element::l -> p(element) && qqsoit(p)(l) |
  [] -> true;;

qqsoit(predicat)([1;2;3;4;5]);; "false"
qqsoit(predicat)([1;2;30;4;5]);; "false"
qqsoit(predicat)([100;20;30;40;50]);; "true"

(* 3- *)
let estMembre = function x -> function liste -> 
  ilexiste(function a -> a=x)(liste) ;; 

estMembre(5)([1;2;3;4]);; "false"
estMembre(3)([1;2;3;4]);; "true"

let estInclus = function l1 -> function l2 -> qqsoit(function e -> estMembre(e)(l2))(l1);;

 estInclus([1;2;3])([1;2;3;4]);; "true"
 estInclus([1;3;5])([1;2;3;4]);; "false"

 (* 4- *)
let rec filter = function p -> ( function 
  e::liste -> if p(e) then e::filter(p)(liste) else filter(p)(liste) |
  [] -> [] );;

filter(predicat)([1;2;3;4]);; "[]"
filter(predicat)([1;20;3;14]);; "[20;14]"

 (* 5- *)
let diffEns = function ens1 -> function ens2 -> filter(function e -> not (ilexiste(function a -> e=a)(ens2)))(ens1);;

"à tester"