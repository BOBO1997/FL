let rec fold_right f lst e =
	match lst with
	| [] -> e
	| a::al -> f a (fold_right f al e);;

let rec fold_left f e lst =
	match lst with
	| [] -> e
	| a::al -> fold_left f (f e a) al;;
