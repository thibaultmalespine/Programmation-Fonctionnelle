(* Exercice 1 *)
(* 1 - *)
let non = function 
true -> false |
false -> true ;;
  (* type : bool -> bool *)
  (* test *)
  non(1 = 1);; (* false *)
  non(1 < 1);; (* true *)

(* 2 - *)
let et = function 
  true, true -> true |
  _ -> false ;;

  (* type : bool * bool -> bool *)
  (* test *)
  et(false, true);; (* false *)
  et(1=1, true);; (* true *)

(* 3 - *)
let nand = function 
  false, false -> true |
  _ -> false ;;
  (* type : bool * bool -> bool *)
  (* test *)
  nand(false, true);; (* false *)
  nand(1=1, true);; (* false *)
  nand(1 = 2, 5 < 4);; (* true *)



(* Exercice 2 *)
(* 1 - *)
let unite = function(x) -> 
    let partie_entiere_positive = int_of_float(abs_float(x)) 
    in partie_entiere_positive mod 10 ;;
  (* type : float -> int *)
  (* test *)
  unite(56.78) ;; (* 6 *)
  unite(-57.14) ;; (* 7 *)

(* 2 - *)
let centieme = function(x) ->
  let n = x *. 100. in let entier_positif = int_of_float(abs_float(n)) in entier_positif mod 10 ;;
  (* type : float -> int *)
  (* test *)
  centieme(56.78);; (* 8 *)
  centieme(-57.14);; (* 4 *)

(* 3 - *)
let echange = function(x,n,p) ->
  let max = function(n,p) -> if n > p then float_of_int(n) else float_of_int(p)
  in let decimale = abs_float(x -. float_of_int(int_of_float(x)))
  in if max(n,p) >= 0. then max(n,p) +. decimale else max(n,p) -. decimale ;;
  (* type : float * int * int -> float *)
  (* test *)
  echange(23.432,8,14);; (* 14.432 *)
  echange(23.432,-8,-14);; (* -8.432 *)
  echange(-23.432,8,14);; (* 14.432 *)
  echange(-23.432,-8,-14);; (* -8.432 *)

(*  - *)
  (* type :  ->  *)
  (* test *)

(* Exercice 3 *)
(* 1 - *)
let fraction = function 
  0, d -> "zéro" |
  n, 1 -> "fraction entière" |
  1, d -> "fraction unaire" |
  n, 0 -> failwith("denominateurNul") |
  n, d -> if ((n >= 0) && (d >= 0) )|| ((n < 0) && (d < 0)) 
          then "fraction positive" 
          else "fraction négative" ;;
  (* type : int * int -> string *)
  (* test *)
  fraction(-2,3) ;; (* fraction négative *)
  fraction(-2,-3) ;; (* fraction positive *)
  fraction(1,4) ;; (* fraction unaire *)
  fraction(5,1) ;; (* fraction entière *)
  fraction(1,1) ;; (* fraction entière *)
  fraction(0,3) ;; (* zéro *)
  fraction(3,0) ;; (* failwith *)

(* 2 - *)
let produit = function((n1,d1),(n2,d2)) -> n1 * n2, d1 * d2 ;;
  (* type : (int * int) * (int * int) -> int * int *)
  (* test *)
  produit(f1,f2);; (* -2,12 *)

(* 3 - *)
let somme = function((n1,d1),(n2,d2)) -> n1 * d2 + n2 * d1, d1 * d2 ;;
  (* type : (int * int) * (int * int) -> int * int *)
  (* test *)
  somme(f1, f2);; (* -5,12 *)
 
(* 4 - *)
let rec pgcd = function
  b, 0 -> b |
  a, b -> pgcd(b, a mod b) ;;
  (* type : int * int -> int *)
  (* test *)
  #trace pgcd ;;
  pgcd(24,18);;
  #untrace pgcd ;;

(* 5 - *)
let simplifie = function(n,d) -> 
  let pgdc = pgcd(n,d) in
  if d < 0 
  then let n = -n and d = -d in n / pgdc, d / pgdc 
  else n / pgdc, d / pgdc ;;
  (* type : int * int -> int * int *)
  (* test *)
  simplifie(24,-18);; (* -4,3 *)
  simplifie(24,18);; (* 4,3 *)

(* 6 - *)
let rec harmo = function
  1 -> 1,1 |
  n -> simplifie(somme((1,n),harmo(n-1)));;
  (* type : int -> int * int *)
  (* test *)
  #trace harmo;;
  harmo(6);;
  #untrace harmo;;
