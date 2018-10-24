let rec fold_right f lst e =
	match lst with
	| [] -> e
	| a::al -> f a (fold_right f al e);;

let rec fold_left f e lst =
	match lst with
	| [] -> e
	| a::al -> fold_left f (f e a) al;;

let append_r x y = 
	let connect a b = a::b
	in fold_right connect x y;;

(*addfの中でfを使いたいので、fold_rightをfilter_lの中で定義*)
let filter_r f lst =
	let addf b ans = if (f b) = true then b::ans else ans
	in let rec fold_right f lst e =
			match lst with
			| [] -> e
			| a::al -> f a (fold_right f al e)
	in fold_right addf lst [];;

let reverse lst =
	let rec sub_reverse lst ans =
		match lst with
		| [] -> ans
		| a::ad -> sub_reverse ad (a::ans)
	in sub_reverse lst [];;

let rec append_l x y =
	let connect a b = b::a
	in fold_left connect y (reverse x);;

(*addfの中でfを使いたいので、fold_leftをfilter_lの中で定義*)
let filter_l f lst = 
	let addf ans b = if (f b) = true then b::ans else ans
	in let rec fold_left f e lst =
			match lst with
			| [] -> e
			| a::al -> fold_left f (f e a) al
	in fold_left addf [] lst;;
