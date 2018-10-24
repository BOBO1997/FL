type 'a tree = 
	| Leaf
	| Node of 'a * 'a tree * 'a tree;;

let level_order tree =
	let rec append t_dim1 t_dim2 =
		match t_dim1 with
		| [] -> t_dim2
		| y::yd -> match t_dim2 with
					| [] -> t_dim1
					| z::zd -> [y@z]@(append yd zd)
	in let rec pre_order tree =
		match tree with
		| Leaf -> []
		| Node (b, c, d) -> 
					[b]
					::
					(append (pre_order c)
							(pre_order d))
	in let rec t_to_o t_dim =
		match t_dim with
		| [] -> []
		| a::ad -> a@(t_to_o ad)
	in match tree with
	| Leaf -> []
	| Node (b, c, d) -> t_to_o (pre_order tree);;
