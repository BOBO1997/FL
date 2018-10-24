type 'a tree = 
	| Leaf
	| Node of 'a * 'a tree * 'a tree;;

(*行きがけ順*)
let rec pre_order tree =
	match tree with
	| Leaf -> []
	| Node (b, c, d) -> [b]@(pre_order c)@(pre_order d);;

(*通りがけ順*)
let rec in_order tree =
	match tree with
	| Leaf -> []
	| Node (b, c, d) -> (in_order c)@[b]@(in_order d);;

(*帰りがけ順*)
let rec post_order tree =
	match tree with
	| Leaf -> []
	| Node (b, c, d) -> (post_order c)@(post_order d)@[b];;
