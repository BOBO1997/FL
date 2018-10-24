let append x y =
	let rec sub_append a d y =
		match d with
		| [] -> a::y
		| f::fd -> a::(sub_append f fd y)
	in match x with
	| [] -> y
	| f::fd -> sub_append f fd y;;

let rec filter f lst =
	match lst with
	| [] -> []
	| a::ad -> if (f a) = true then a::(filter f ad) else (filter f ad);;
