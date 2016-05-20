type arbre = F of int | N of arbre * arbre 

(* nombre de noeuds *)
let n = 2

let fact n = 
	let rec factorielle x acc =
		match x with
		| 0 -> acc
		| a -> factorielle (x-1) (acc*a)
	in
	factorielle n 1

(* (2n)!/(n+1)!n!  *)
let catalan n = 
	Printf.printf "n : %d" n; 
	flush stdout;
	match n with
	| 0 -> 1 
	| _ -> fact(2*n)/(fact(n+1)*fact(n))
	

let compteur () =
	let etat = ref 0 in
	fun () -> etat := !etat + 1; !etat 

let unrank index =

	let rec paire reste (gauche,droite) =
		Printf.printf "reste %d gauche %d droite %d \n " reste gauche droite;
		flush stdout;
		let cata = catalan gauche * catalan droite in 
		if reste > cata then paire (reste-cata) (gauche-1, droite+1)
		else (gauche, droite)
	in


	let rec ur nbr i =
		match nbr with
		| 0 -> F(0)
		| 1 -> N(F(0), F(0))
		| a -> let (left, right) = paire i (nbr-1, 0) in
		let c = compteur () in 
		Printf.printf "left : %d , right : %d \n" left right ;
		flush stdout;
		if c() > 0 then
		N(ur left ((i mod left)+1), ur right ( ( i / left ) +1 ) )
		else F(0)
	in


	ur n index


let count = catalan n

let rec affiche_arbre a=
	match a with
	| F(a) -> Printf.printf "F(%d)" a
	| N(g,d) -> Printf.printf "N(" ; affiche_arbre g; Printf.printf ","; affiche_arbre d; Printf.printf ")"

let () =
	let test = unrank 3 in affiche_arbre test