(* Exercice 1 *)
  (* 1- *)
let rec min = function 
  a::b::liste -> if b < a then min(b::liste) else min(a::liste) |
  a::[] -> a |
  [] -> failwith("la liste ne doit pas être vide !");;
  
min([5;7;8;2;9]);; (* 2 *)
min([1;7;8;2;9]);; (* 1 *)
min([1;7;8;2;-5]);; (* -5 *)


  (* 2- *)
let rec enleve = function
  x, a::liste -> if x = a then liste else a::enleve(x, liste) |
  x, [] -> [];;

enleve(7, [5;7;8;7;9]);; (* [5;8;7;9] *)
enleve(1, [1;7;8;2;9]);; (* [7;8;2;9] *)
enleve(9, [1;7;8;2;9]);; (* [1;7;8;2] *)


  (* 3- *)
let rec naif = function
  [] -> [] | 
  liste -> let mini = min(liste) 
    in mini::naif(enleve(mini, liste));;
 
naif([5;7;8;2;9]);; (* [2;5;7;8;9] *)
naif([1;7;8;2;-5]);; (* [-5;1;2;7;8] *)


(* Exercice 2 *)
  (* 1- *)
let rec insertion = function 
  x, a::liste -> if x < a then x::a::liste else a::insertion(x, liste) |
  x, [] -> x::[];;

insertion(5, [2;4;8;10]);; (* [2;4;5;8;10] *)
insertion(1, [2;4;8;10]);; (* [1;2;4;8;10] *)
insertion(11, [2;4;8;10]);; (* [2;4;8;10;11] *)

  (* 2- *)
let rec tri_insere = function 
  a::b::liste -> insertion(a, tri_insere(b::liste)) |
  a::[] -> a::[] |
  [] -> failwith("la liste ne doit pas être vide !");;

tri_insere([5;7;8;2;9]);; (* [2;5;7;8;9] *)
tri_insere([1;7;8;2;-5]);; (* [-5;1;2;7;8] *)
    

(* Exercice 3 *)
  (* 1- *)
(*Quelle stratégie algorithmique est ici utilisée ?*) (* Diviser pour régner *)
let rec divise = function 
  a::b::liste -> let t1,t2 = divise(liste) in a::t1, b::t2 |
  a::liste -> let t1,t2 = divise(liste) in a::t1, [] |
  [] -> [], [];; 
  

divise([7;5;3;2;4;8]);; (*[7; 3; 4], [5; 2; 8]*)

  (* 2- *)
let rec fusion = function 
  a::liste1, b::liste2 -> if a < b 
                          then a::fusion(liste1, b::liste2)  
                          else b::fusion(a::liste1, liste2) |
  [], liste2 -> liste2 |
  liste1, [] -> liste1 ;;

fusion([1;2;3],[4;5;6]);; (* [1;2;3;4;5;6] *)
fusion([1;3;5],[2;4;6]);; (* [1;2;3;4;5;6] *)
fusion([4;5;6],[1;2;3]);; (* [1;2;3;4;5;6] *)

  (* 3- *)
let rec tri_fusion = function
  [] -> failwith("la liste ne doit pas être vide !") |
  a::[] -> a::[] |
  liste -> let t1, t2 = divise(liste) in fusion(tri_fusion(t1), tri_fusion(t2));;

tri_fusion([5;7;8;2;9]);; (* [2;5;7;8;9] *)
tri_fusion([1;7;8;2;-5]);; (* [-5;1;2;7;8] *)