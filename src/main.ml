type arbre = F of int | N of arbre * arbre 

(* nombre de noeuds *)
let n = 3 

let fact n = 
	let rec factorielle x acc =
		match x with
		| 0 -> acc
		| a -> factorielle (x-1) (acc*a)
	in
	factorielle n 1


(* (2n)!/(n+1)!n!  *)
let count = fact(2*n)/(fact(n+1)*fact(n))

let () =