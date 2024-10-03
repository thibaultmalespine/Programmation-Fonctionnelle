(* Exercice 1 *)

let rec somme = function
  0 -> 0 |  
  n -> n + somme(n-1);;


(* Exercice 2 *)

let rec puissance = function 
  (x, 0) -> 1 |
  (x, p) -> x * puissance(x, p-1);;
  

(* Bonus *)

let rec factorielle = function  
  0 -> 1 |
  x -> x * factorielle(x-1);;

(* Exercice 3 *)
let rec repet = function
  c,0 -> 0 |
  c,n -> c * puissance(10,n-1) + repet(c, n-1);;  

let rec unChiffre = function(n,c) ->
  if (n = c) then true else if (n < c) then false
  else (n mod 10) = c && unChiffre(n/10,c);;

let rec pgd = function
  n,1 -> 1 |
  n,d -> if (n mod d) = 0 then d else pgd(n, d-1);;
  
let rec nbPairChif = function
  0 -> true |
  n -> if (n < 10) then false else nbPairChif(n/100);; 
    

(* Exercice 4 *)
let rec mult_egypt = function 
  (1, p) -> p |
  (n, p) -> if (n mod 2) <> 0 then p + mult_egypt(n-1, p)
          else mult_egypt(n/2, 2*p);;

(* type : int * int --> int *)
mult_egypt(5,4) (* - : int = 20 *)

(* Exercice 5 *)
let rec s_chif = function n ->
          if n < 10 then n 
          else n mod 10 + s_chif(n/10);; 


let rec som_chif = function n ->
  if s_chif(n) < 10 then s_chif(n)
  else som_chif(s_chif(n));;

(* Exercice 6 *)
  (* 1- *)
let majuscule = function c -> c < 'a' && c > '9';;

let minuscule = function c -> c > 'Z';;

let lettre = function c -> c < 'A';;

  (* 2- *)

let rec appartient = function 
  c, "" -> false |
  c, s -> if (c = tetec(s)) then true 
          else appartient(c, reste(s));; 

(* type : char * string -> bool *)

(* test *)
appartient('a', "savoir") (* true *)
appartient('n', "oui") (* false *)

  (* 3- *)

let rec debut = function 
  s1, "" -> true |
  "", s2 -> true |
  s1, s2 -> if (tetec(s1) = tetec(s2)) then debut(reste(s1), reste(s2))
            else false ;;

(* type : string * string -> bool *)

(* tests *)
debut("debut", "deb") (* true *)
debut("te", "test") (* true *)
debut("oui", "oulà") (* false *)


  (* 4- *)

let rec incluse = function 
    "",s2 -> false |
    s1,s2 -> debut(s1, s2) || incluse(reste(s1), s2);;

(* type : string * string -> bool *)

(* test *)
incluse("chaine_de_caractère", "act");; (* true *)
incluse("chaine_de_caractère", "zèbre");; (* false *)


  (* 5- *)

let rec frequence = function 
  c,"" -> 0 | 
  c, s -> if tetec(s) = c 
          then 1 + frequence(c , reste(s))
          else frequence(c , reste(s)) ;; 

(* type : char * string -> int *)

(* test *)
frequence('c', "occurrence");; (*3*)
frequence('z', "occurrence");; (*0*)


  (* 6- *)
  
let rec elimine = function 
      c, "" -> "" |
      c, s -> if tetec(s) <> c
              then tetes(s) ^ elimine(c, reste(s))
              else elimine(c, reste(s));; 

(* type : char * string -> string *)

(* test *)

elimine('c', "occurrence");; (* ourrene *)
elimine('z', "occurrence");; (* occurrence *)


  (* 7- *)

let rec renverse = function 
        "" -> "" |
        s -> renverse(reste(s)) ^ tetes(s);;

(* type : string -> string *)

(* test *)

renverse("stylo");; (* olyts *)
renverse("kayak");; (* kayak *)


(* 8- *)

let palindrome = function(s) -> if renverse(s) = s then true else false ;;

(* type : string -> bool *)

(* test *)

palindrome("stylo");; (* false *)
palindrome("kayak");; (* true *)


(* Exercice 7 *)
   (* 1- *)
let chiffre = function 
    'I' -> 1 |
    'V' -> 5 |
    'X' -> 10 ;;

chiffre('V') ;; (*- : int = 5 *)


let rec rom1 = function
    "" -> 0 |
    s -> chiffre(tetec(s)) + rom1(reste(s)) ;; 

  rom1 "XXVIII" ;; (* - : int = 28 *)


  (* 2- *)
let valeurI = function
  'I' -> 2 |
  'V' -> 4 |
  'X' -> 9 ;;

  valeurI 'X' ;; (*- : int = 9*)


let rec romain = function
   "" -> 0 |
  s -> let t = tetec(s) and r = reste(s) in
        if tetec(s) = 'I' then
          let suivant = tetec(r) in
          valeurI(suivant) + romain(reste(r))
        else chiffre(t) + romain(r);;

romain "XXIV" ;; (*- : int = 24*)
romain "XXXIX" ;; (*- : int = 39*)