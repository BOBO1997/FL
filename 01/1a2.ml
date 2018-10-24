let reverse lst =
	let rec sub_reverse lst ans =
		match lst with
		| [] -> ans
		| a::ad -> sub_reverse ad (a::ans)
	in sub_reverse lst [];;

let fold_left2 f e lst =
	let g a b = f b a
	in let rec fold_right f lst e =
		match lst with
		| [] -> e
		| a::al -> f a (fold_right f al e)
	in fold_right g (reverse lst) e;;

let fold_right2 f lst e =
	let g a b = f b a
	in let rec fold_left f e lst =
		match lst with
		| [] -> e
		| a::al -> fold_left f (f e a) al
	in fold_left g e (reverse lst);;
