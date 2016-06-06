type arbre = F | N of arbre * arbre 

(* nombre de noeuds *)
let n = 6

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

		if droite = n-1 then 
			(reste, gauche, droite) 
		else
			let cata = catalan gauche * catalan droite in 

			if reste > cata then  
 				paire (reste-cata) (gauche-1, droite+1) 
			else
				(reste, gauche, droite) 
	in

	let rec ur nbr i =
		match nbr with
		| 0 -> F
		| 1 -> N(F, F)
		| a -> let (reste,left, right) = paire i (nbr-1, 0) in

			if right > 0 then 	
			   N(ur left (reste / (catalan right)), ur right (1+((reste-1) mod catalan right))) 
			else
			   N(ur left i, ur 0 0)
	in

	ur n index

let rec compte_n arbre =
	match arbre with
	| F -> 0
	| N(g,d) -> 1 + compte_n g + compte_n d

let somme_cat nbr =
	let i = ref nbr in 
	let tmp = ref 0 in

	while !i <> n-1 do 
		tmp := !tmp + catalan (!i+1) * catalan (n-2-(!i));
		incr i;
	done;

	!tmp

let rec rank arbre = 
	let rec rk abr acc = 
		match abr with
		| N(N(F,F), F) -> acc
		| N(F, N(F,F)) -> acc+1
		| N(g, F) -> rk g acc
		| N(g, d) -> let c_gauche = compte_n g in 
		             let min = somme_cat c_gauche in 
		             rk d (acc+min)
		| F -> acc
	in

	rk arbre 0

let rec lisG ?(enleve = false) ?(ajoute = false) arbre =

	if enleve then 
		match arbre with
		| N(g, d ) -> N(F, lisG d)
		| _ -> Printf.printf"what \n" ; raise Exit (*Ne doit pas arriver !!!*)
	else if ajoute then
		N(lisG arbre, F)
	else
	match arbre with
	| F -> F
	| N(F, d) -> N(lisG d, F)
	| N(g, d) -> N(lisG g, lisG d)


let rec next arbre =
	Printf.printf "test next \n";
	match arbre with
	 | F -> raise Exit (*n'existe pas de suivant avec cette branche*)
	 | N(F, F) -> raise Exit (*n'existe pas de suivant avec cette branche *)
	 | N(g, d) -> try 
	 				N(g, next d) (*on essaye de trouver un suivant avec la branche droite*)
	 			with exit ->
	 				try 
	 					N(next g, lisG d) (* puis avec la branche gauche*)
	 				with exit ->
	 					match g with (*on regarde si on peut creer un suivant*)
	 					| F -> raise Exit  (*on peut rien faire*)
	 					| _ -> Printf.printf "bonjour \n" ;N(lisG ~enleve: true g, lisG ~ajoute: true d) 
	 					(*on peut en retirant un noeud de gauche et en le mettant à droite*)
	 						
(* 
let rec previous arbre = 
	match arbre with
	| F -> raise Exit
	| N(F, F) -> raise Exit
	| N(F, d) -> N(F, previous d)
	| N(g, d) -> try 
					N(previous g, d)
				with exit -> N(g, rotationG d) (*pareil, peut renvoyer exit*) *)

let rec affiche_arbre a=
	match a with
	| F -> Printf.printf "F" 
	| N(g,d) -> Printf.printf "N(" ; affiche_arbre g; Printf.printf ","; affiche_arbre d; Printf.printf ")"

let () =

(* 	Printf.printf "nbr %d\n" count
 *)

	let rec test_next elem cpt =
		if cpt = count then 
			() 
		else

			affiche_arbre elem;
			Printf.printf "       ";
			affiche_arbre (unrank cpt);
			Printf.printf "\n\n";

			let nwelem = next elem in 

			test_next nwelem (cpt+1)
	in

	test_next (unrank 1) 1