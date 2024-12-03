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
let diffEns = function ens1 -> function ens2 -> 
  filter(function e -> not (ilexiste(function a -> e=a)(ens2)))(ens1);;

"à tester"
diffEns([1;2;3;4;5])([2;3;5]);;
- int list = [1;4]

diffEns([1;2;5])([2;3;5]);;
- int list = [1]


(* Exercice 3 *)
(* PARTIE 1 *)
(* 1- *)
type expression = Const of int 
| Var of char
| Add of expression*expression
| Mult of expression*expression
| Puiss of expression*int ;;

let e1 = Add (Const 1, Mult(Const 2, Puiss(Var 'x',3)));; 


type liaison = {id : char ; valeur : int} ;;

let envC = [{id = 'a' ;valeur = 3} ;{id = 'b' ;valeur = 4}] ;;

(* 2- *)
let rec evalVar = function 
    c, liaison::env -> if liaison.id = c then liaison.valeur else evalVar(c,env) |
    c, [] -> failwith("identificateur inconnu");;

evalVar ('a',envC) ;;
- : int = 3
evalVar ('b',envC) ;;
- : int = 4
evalVar ('x',envC) ;;
Exception " identificateur inconnu"

(* 3- *)

let rec puissance = function n, puiss -> 
  if puiss > 0 then n * puissance(n, puiss-1)   
  else if puiss = 0 then 1 else failwith("la puissance doit être positive ou nulle");;

puissance(2,3);;
- : int = 8

puissance(10,0);;
- : int = 1

puissance(8,-1);;
Exception "la puissance doit être positive ou nulle"


(* 4- *)

let evalExp = function env -> function 
  expr -> let rec evalE = ( function
    Const valeur -> valeur |
    Var var -> evalVar(var, env) |
    Add (expr1, expr2) -> evalE(expr1) + evalE(expr2) |
    Mult (expr1, expr2) -> evalE(expr1) * evalE(expr2) |
    Puiss (expr, puiss) -> puissance(evalE(expr), puiss)
  ) in evalE(expr)
;;
(* PARTIE 2 *)
evalExp(envC)(e1);;
(* 1- *)

type definition = {ident :char ; exp : expression } ;;

let ajoute = function envC -> function def -> (
  let valeur = evalExp(envC)(def.exp) 
    in {id = def.ident ; valeur = valeur }::envC 
);;

ajoute envC {ident='x' ;exp = Add(Var 'a',Const 3)} ;;
- : liaison list = [{id = 'x' ; valeur = 6} ; {id = 'a' ; valeur = 3} ; {id = 'b' ; valeur = 4}]


(* PARTIE 3 *)

type programme =
Elementaire of expression
| DefGlob of definition
| DefLocale of definition*programme ;;


let p = DefLocale ({ident = 'x' ; exp = Const 7},
  DefLocale ({ident = 'y' ; exp = Add (Var 'x', Const 3)},
  Elementaire (Add (Var 'x', Mult (Const 3, Var 'y'))))) ;;


let rec evalProg = function prog, env -> (match prog with 
  Elementaire expr -> evalExp(env)(expr), env |
  DefGlob def -> evalExp(env)(def.exp), ajoute(env)(def) |
  DefLocale (def, p) -> let res, newEnv = evalProg(p, ajoute(env)(def)) in  
    res, env
);;

let e2 = Add (Const 1, Mult(Var 'a', Const 2));;
evalProg( Elementaire e2,envC) ;;
- : int * liaison list = 7, [{id = 'a' ; valeur = 3} ; {id = 'b' ; valeur = 4}]

evalProg( DefGlob {ident='x' ; exp=e2 },envC) ;;
- : int * liaison list = 7, [{id = 'x' ; valeur = 7} ; {id = 'a' ; valeur = 3} ; {id = 'b' ; valeur = 4}]

evalProg ( p ,envC) ;;
- : int * liaison list = 37, [{id = 'a' ; valeur = 3} ; {id = 'b' ; valeur = 4}]


(* Exercice 4 *)

let rec sigma = function f -> (function 
  i when i > 0 -> f(i) + sigma(f)(i-1) |
  i when i = 0 -> f(0) |
  _ -> failwith("le paramètre i doit être supérieur à 0"));; 

sigma(function i -> i*i)(10);;

let sigma = function f -> (function i ->
  let rec localRec = (function 
    i when i > 0 -> f(i) + localRec(i-1) |
    i when i = 0 -> f(0) |
    _ -> failwith("le paramètre doit être supérieur à 0")) in localRec(i)
);;
sigma(function i -> i)(10);;


(* BONUS *)
(* Exemple *)
exception NombreNegatif;;

let rec fact = function
0->1
| n-> if n<0 then raise NombreNegatif
else n*fact(n-1) ;;

fact(-3) ;;

(* Exercice *)
exception NonTrouve;;

let rec associe = function a -> function 
  (_a ,_b)::liste when a = _a -> _b |
  (_a ,_b)::liste -> associe(a)(liste) |
  [] -> raise NonTrouve;;

associe(1)([(3,2);(1,4)]);; (* 4 *)
associe(1)([(3,2);(7,4)]);; (* NonTrouvé *)

(* Capture d'exception *)

(* 1- *)
exception PasTrouve;;

(* 2- *)
let rec trouve = function a -> function 
  (_a ,_b)::liste when a = _a -> _b |
  (_a ,_b)::liste -> trouve(a)(liste) |
  [] -> raise PasTrouve;;

let dico= [("a","un") ; ("called","appelé") ;
  ("cat","chat") ; ("hand","main") ;
  ("is","est") ; ("language","langage") ;
  ("my","mon") ; ("us","nous") ;
  ("wonderful","magnifique")] ;;
  

trouve("cat")(dico);;
trouve "dog" dico ;;

(* 3- *)
let traduire = function mot -> try trouve(mot) with PasTrouve -> mot;;
