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