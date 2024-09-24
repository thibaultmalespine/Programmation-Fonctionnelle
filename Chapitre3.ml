(* Exercice 2 *)

  (* Question 3 *)
let decimales = function x -> 
  let troncature = int_of_float(x) in x -. float_of_int(troncature);;

  (* Question 4 *)
let plus_proche_entier = function x ->
    if x >= 0.0 
      then int_of_float(x +. 0.5) 
      else - int_of_float(abs_float(x) +. 0.5);; 

  (* Question 5 *)
let arrondi = function x ->
  float_of_int(plus_proche_entier(x *. 100.)) *. 0.01;;
      
  (* Question 6 *)
let francs_en_euros = function x ->
  arrondi(x /. 6.55957);;



(* Exercice 3 *)
  (* Question 1 *)
let heures = function x -> 
  int_of_float(x);;

let minutes = function x ->
  let decimales = x -. float_of_int(heures(x)) 
    in plus_proche_entier(decimales *. 100.) ;;

  (* Question 2 *)
let quelle_heure_est_il = function x -> 
  let debut = "Il est " and heures = string_of_int(heures(x)) and minutes = string_of_int(minutes(x))
    in
  let milieu = if heures = "12" then "midi " else if heures = "0" then "minuit " else heures ^ " heure "
    in
  let fin = if minutes = "0" then "pile." else minutes
    in
  debut ^ milieu ^ fin;;


(* Exercice 4 *)
let min = function (x,y) ->
  if x < y then x else y;;

let norme = function (x,y,z) -> 
  let carree = function x -> x*x 
    in
  carree(x) + carree(y) + carree(z);;


(* Exercice 5 *)
let reel = function (x,y,r) ->
  let max = function (x,y) -> if x>y then x else y
    in 
  float_of_int(max(x,y)) +. abs_float(r) -. abs_float(float_of_int(int_of_float(r)));; 
  
(* Exercice 6 *)
let chiffre = function (n) -> n mod 10;;

let echange = function(n,p) -> 
  let n2 = ((n) - (n mod 10)) 
    in 
  let p2 = if n2 >= 0 then (p mod 10) else - (p mod 10)
    in
  n2 + p2 ;;


(* Exercice 7 *)

let abcEgaux = function(a,b,c) -> (a = b) && (b = c);;
let abEgaux = function(a,b,c) -> (a = b) && (a != c);;
let b_entre_a_et_c = function(a,b,c) -> (a < b) && (b < c);;
let deux_valeurs_au_moins_identiques = function(a,b,c) -> (a = b) || (b = c) || (a = c);;
let deux_valeurs_strictement_identiques = function(a,b,c) -> (a = b) && (a != c) || (b = c) && (b != a) || (a = c) && (a != b);;
let deux_valeurs_au_plus_identiques = function(a,b,c) -> (a != b) || (b != c) || (a != c);;


(* Exercice 8 *)

let nbDeSolutions = function(a,b,c) -> 
  let calculDelta = function(a,b,c) -> b*b - 4*a*c
    in
  if calculDelta(a,b,c) > 0 then 2
  else if calculDelta(a,b,c) = 0 then 1
  else 0;;
