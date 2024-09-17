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

(**)
  let x = 2;;
  let add = function y -> x+y;;
  let x = 4;; 
