(* Exercice 1 *)
let f = function n -> (((n mod 2) = 1 && (n mod 7) = 0) || ((n mod 2) = 0 && n < 15)) ;;
                     
  (* Type : int -> bool *)
  (* TEST *)  
f(7);; (* true *) (* impair et multiple de 7 *)
f(-4);; (* true *) (* pair et inférieur à 15 *)
f(16);; (* false *) (* pair et supérieur à 15 *)
f(-1);; (* false *) (* impair et non multiple de 7 *)


(* Exercice 2 *)
  (* 1 - *)
let chiffre = function n -> n mod 10 ;;
  (* Type :  int -> int *)
  (* TEST *)
chiffre(0) ;; (* 0 *)
chiffre(758) ;; (* 8 *)
chiffre(-758) ;; (* -8 *)

  (* 2 - *)
let echange = function n,p -> let last_n = chiffre(n) and last_p = chiffre(p)
  in n - last_n + last_p , p - last_p + last_n ;;
  (* Type :  int * int -> int * int *)
  (* TEST *)  
echange(145,163);; (* 143, 165 *)


(* Exercice 3 *)
let rec triangulaire = function 
  1 -> 1 |  
  n -> if n > 0 then n + triangulaire(n-1) else failwith("paramètre négatif ou nul") ;; 
  (* Type :  int -> int *)
  (* TEST *)
triangulaire(4);; (* 10 *)
triangulaire(-2);; (* failwith *)


(* Exercice 4 *)
  (* 1 - *)
let ajout_en_fin = function  
    "" -> failwith("la chaîne de caractère est vide") |
    s -> reste(s) ^ tetes(s) ;; 
  (* Type : string -> string *)
  (* TEST *)
ajout_en_fin("bonjour");; (* onjourb *)
ajout_en_fin("");; (* failwith *)

  (* 2 - *)
let rec rotation = function
  chaine, 1 -> ajout_en_fin(chaine) |
  chaine, n -> rotation(ajout_en_fin(chaine), n-1);;
  (* Type : string * int -> string *)
  (* TEST *)
rotation("bonjour",3);; (* jourbon *)


(* Exercice 5 *)
  (* 1 - *)
let rec diviseurs = function
  n, 1 -> 1 |
  n, k -> if n mod k = 0 then 1 + diviseurs(n, k-1) else diviseurs(n, k-1) ;;
  (* Type : int * int -> int *)
  (* TEST *)
diviseurs(6,5);; (* 3 *)

  (* 2 - *)
let premier = function n -> diviseurs(n,n) = 2 ;;
  (* Type : int -> bool *)
  (* TEST *)
premier(7);; (* true *)
premier(8);; (* false *)

  (* 3 - *)
let rec max_diviseurs = function 
  1 -> 1,1 |
  n -> let max = function ((n1,p1),(n2,p2)) -> if p2 > p1 then n2,p2 else n1,p1
    in max((n, diviseurs(n,n)), (max_diviseurs(n-1)));;
  (* Type : int -> int * int *)
  (* TEST *)
max_diviseurs(7);; (* 6,4 *)
