type arbre = F | N of arbre * arbre 

(* nombre de noeuds *)
let n = 8

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

let rotationD arbre = 
	match arbre with
	| N(N(sg,sd),d) -> N(sg,N(sd,d))
	| _ -> raise Exit (*rotation impossible*)

let rec next arbre =
	match arbre with
	 | N(F, F) -> raise Exit
	 | N(g, F) -> N(next g, F) (*a verifier*)
	 | N(g, d) -> try 
	 				N(g, next d)
	 			with exit -> N(rotationD g, d) (*si rotation plante, ca renvoie exit qd mme*)
	 						
let rotationG arbre = 
	match arbre with
	| N(g,N(sg,sd)) -> N(N(g,sg),sd)
	| _ -> raise Exit (*rotation impossible*)

let rec previous arbre = 
	match arbre with
	| N(F, F) -> raise Exit
	| N(F, d) -> N(F, previous d)
	| N(g, d) -> try 
					N(previous g, d)
				with exit -> N(g, rotationG d) (*pareil, peut renvoyer exit*)

let rec affiche_arbre a=
	match a with
	| F -> Printf.printf "F" 
	| N(g,d) -> Printf.printf "N(" ; affiche_arbre g; Printf.printf ","; affiche_arbre d; Printf.printf ")"

let () =

(* 	Printf.printf "nbr %d\n" count
 *)
	for i = 1 to catalan n do 
		let test = unrank i in Printf.printf "\n\n"; affiche_arbre test
	done  