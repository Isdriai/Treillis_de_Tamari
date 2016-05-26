type arbre = F | N of arbre * arbre 

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


let rec affiche_arbre a=
	match a with
	| F -> Printf.printf "F" 
	| N(g,d) -> Printf.printf "N(" ; affiche_arbre g; Printf.printf ","; affiche_arbre d; Printf.printf ")"


let () =

	for i = 1 to catalan n do 
	let test = unrank i in Printf.printf "\n\n"; affiche_arbre test
	done 

	
