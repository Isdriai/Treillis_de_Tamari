type arbre = F of int | N of arbre * arbre 

(* nombre de noeuds *)
let n = 4

let fact n = 

	let rec factorielle x acc =
		match x with
		| 0 -> acc
		| a -> factorielle (a-1) (acc*a)
	in
	factorielle n 1

(* (2n)!/(n+1)!n!  *)
let catalan n = 
	match n with
	| 0 -> 1 
	| _ -> fact(2*n)/(fact(n+1)*fact(n))
	
let count = catalan n


let unrank index =

	let rec paire reste (gauche,droite) =
(* 		Printf.printf "reste %d" reste;
		flush stdout; *)
		if droite = n-1 then 
			(gauche, droite) 
		else
			let cata = catalan gauche * catalan droite in 

			if reste > cata then  
 				paire (reste-cata) (gauche-1, droite+1) 
			else  
				(gauche, droite) 
	in

	let rec ur nbr i =
(* 		Printf.printf "nbr %d \n" nbr;
		flush stdout; *)
		match nbr with
		| 0 -> F(0)
		| 1 -> N(F(0), F(0))
		| a -> let (left, right) = paire i (nbr-1, 0) in

(* 			Printf.printf " left : %d right %d  index %d \n " left right i ;
			flush stdout; *)

			if right > 0 then
			   N(ur left (i / (catalan right)), ur right ( 1+(i mod (catalan right)) ))
			else
			   N(ur left i, ur 0 0)
	in

	ur n index


let rec affiche_arbre a=
	match a with
	| F(a) -> Printf.printf "F(%d)" a
	| N(g,d) -> Printf.printf "N(" ; affiche_arbre g; Printf.printf ","; affiche_arbre d; Printf.printf ")"


let () =
	let test = unrank (int_of_string Sys.argv.(1)) in affiche_arbre test
