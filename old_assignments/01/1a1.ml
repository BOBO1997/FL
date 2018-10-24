let rec fold_right f lst e =
	match lst with
	| [] -> e
	| a::al -> f a (fold_right f al e);;

let reverse lst =
	let rec sub_reverse lst ans =
		match lst with
		| [] -> ans
		| a::ad -> sub_reverse ad (a::ans)
	in sub_reverse lst [];;

let reverser lst =
	let f x y z= y (x::z)
	in (fold_right f lst (fun x -> x)) [];;
