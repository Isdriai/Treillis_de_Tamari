type arbre = F | N of arbre * arbre 

let rec affiche_arbre a=
	match a with
	| F -> Printf.printf "F" 
	| N(g,d) -> Printf.printf "N(" ; affiche_arbre g; Printf.printf ","; affiche_arbre d; Printf.printf ")"

let affiche_sup a =
	let rec aff s = function
	| F -> Printf.printf "%s" s; Printf.printf "F\n"
	| N (d, g) -> let s2 = s^"|            " in
		 aff s2 g; Printf.printf "%s" s; Printf.printf"N\n"; aff s2 d 
	in
	aff "" a

let rec test_nav liste =
	match liste with
	| a::b -> Printf.printf "\n\n";
			  affiche_sup a;
			  test_nav b
	| [] -> ()

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
			   N(ur left (1+((reste-1) / (catalan right))), ur right (1+((reste-1) mod catalan right))) 
			else
			   N(ur left i, ur 0 0)
	in

	ur n (index+1)

let rec compte_n arbre =
	match arbre with
	| F -> 0
	| N(g,d) -> 1 + compte_n g + compte_n d

let somme_cat nbr niv =
	let i = ref nbr in 
	let tmp = ref 0 in
	incr i;

	while !i <> niv do 
		tmp := !tmp + catalan (!i) * catalan (niv-1-(!i));
		incr i;
	done;

	!tmp

let rec rank arbre = 
	let rec rk abr niv= 
		match abr with
		| F -> 0
		| N(g,d) -> let n_gauche = compte_n g in
					let n_droite = niv-1-n_gauche in 
					let min = somme_cat n_gauche niv in 
  					min + (rk d n_droite) + ((catalan n_droite)*(rk g n_gauche))
 				
	in

  	rk arbre n

let rec lisG ?(enleve = false) ?(ajoute = false) arbre =

	if enleve then 
		match arbre with
		| N(_, d ) -> lisG d
		| _ -> raise Exit (*Ne doit pas arriver !!!*)
	else if ajoute then
		N(lisG arbre, F)
	else
	match arbre with
	| F -> F
	| N(F, d) -> N(lisG d, F)
	| N(g, d) -> N(lisG g, lisG d)

let rec next arbre =
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
	 					| _ -> N(lisG ~enleve: true g, lisG ~ajoute: true d) 
	 					(*on peut en retirant un noeud de gauche et en le mettant à droite*)
	 
let rec lisD ?(enleve = false) ?(ajoute = false) arbre =
	if enleve then
		match arbre with
		| N(g,_) -> lisD g
		| _ -> raise Exit
	else if ajoute then
		N(F, lisD arbre)
	else
	match arbre with
	| F -> F 
	| N(g, F) -> N(F, lisD g)
	| N(g, d) -> N(lisD g, lisD d)

let rec previous arbre =
	match arbre with
	| F -> raise Exit
	| N(F, F) -> raise Exit
	| N(g, d) -> try
					N(g, previous d)
				with exit ->
					try
						N(previous g, lisD d)
					with exit -> 
						match d with
						| F -> raise Exit
						| _ -> N(lisD ~ajoute: true g, lisD ~enleve: true d)

let rec succ arbre =
 	(* navigation arbre rotationD *)
 	match arbre with
 	| F -> []
 	| N(F ,a) -> List.map (fun ra -> N(F, ra) ) ( succ a)
 	| N(N(a,b) as g, c) -> N(a, N(b, c)) :: 
 						   List.rev_append 
 						   (List.map (fun rg -> N(rg, c)) (succ g)) 
 						   (List.map (fun rd -> N(g, rd)) (succ c))

let rec prec arbre =
	(* navigation arbre rotationG *)
	match arbre with
	| F -> []
	| N(a, F) -> List.map (fun ra -> N(ra, F)) (prec a)
	| N(a, (N(b, c) as d)) -> N(N(a,b), c) ::
							List.rev_append
							(List.map (fun rg -> N(rg, a)) (prec d))
							(List.map (fun rd -> N(d ,rd)) (prec a))

let () =

 	Printf.printf "nbr %d\n" count;
 	flush stdout;
 
(* 
  	let rec test_next elem cpt =

			Printf.printf " nouvelle etape \n";
			affiche_arbre elem;
			Printf.printf "       ";
			let urk = unrank cpt in 
			affiche_arbre urk;

			Printf.printf "     %b       %d" (urk=elem)  cpt;
			Printf.printf "\n\n";

			flush stdout;

			let nwelem = next elem in 

			test_next nwelem (cpt+1)
	in

	test_next (unrank 0) 0   *)
 
 
(* 	let rec test_previous elem cpt =

		Printf.printf "nouvelle etape\n";
		affiche_arbre elem;
		Printf.printf "       ";
		let urk = unrank cpt in 
		affiche_arbre urk;

		Printf.printf "     %b    %d" (urk=elem) cpt;

		Printf.printf "\n\n";
		let nwelem = previous elem in 

		test_previous nwelem (cpt-1)
	in 

	test_previous (unrank count) count  *) 

(* let truc = unrank 113 in 
 			Printf.printf "\n\n";
affiche_sup truc;
Printf.printf "\n\n";
affiche_sup (previous truc);
 
Printf.printf "\n\n";
affiche_sup (unrank 112) *)
(* 
    let rec test_rank elem cpt =
	if cpt = count  then 
		()
	else
		let rk = rank elem in 
		Printf.printf "cpt %d   rank %d     %b \n" cpt rk (rk=cpt);
		test_rank (unrank (cpt+1)) (cpt+1)
in 

test_rank (unrank 0) 0  *)
  
(* 
let test = unrank 61 in 

let rk = rank test in ()     *)



let test = N ( N ( F , N ( N ( F, F), F)), N(F, N(N(F,F), F))) in 

let run = prec test in 

Printf.printf "resultat final \n\n\n";

test_nav run