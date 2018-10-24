(*insert は [[]] を返す*)
let rec insert pre a post =
	match post with
	| [] -> [pre@[a]]
	| y::yd -> (pre@[a]@post)::(insert (pre@[y]) a yd);;

(*insert_full は [[]] を返す*)
(*insert_fullは部分的なpermitationの完成,つまり、ansを@する作業*)
let rec insert_full a two_dim =
	match two_dim with
	| [] -> []
	| y::yd -> (insert [] a y)@(insert_full a yd);;(*二次元リスト同士の連結*)

(*sub_perm は ans を返す*)
(*sunb_permはlstを分割して再帰させる役割*)
let rec sub_perm lst ans =
	match lst with
	| [] -> ans
	| a::ad -> insert_full a (sub_perm ad ans);;

let rec perm lst = sub_perm lst [[]];;
