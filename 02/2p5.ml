type value = VInt of int | VBool of bool;;

type expr =
	| EConstInt of int
	| EAdd of expr * expr
	| ESub of expr * expr
	| EMul of expr * expr
	| EDiv of expr * expr
	| EConstBool of bool
	| EEqual of expr * expr
	| ELessThan of expr * expr
	| EIfThenElse of expr * expr * expr;;

exception Eval_error;;

let rec eval a =
	match a with
	| EConstInt b -> VInt b
	| EAdd (b, c) -> VInt (match (eval b) with 
							| VInt d -> (match (eval c) with 
										| VInt e -> d + e
										| VBool e -> raise Eval_error)
							| VBool d -> raise Eval_error)
	| ESub (b, c) -> VInt (match (eval b) with 
							| VInt d -> (match (eval c) with 
										| VInt e -> d - e
										| VBool e -> raise Eval_error)
							| VBool d -> raise Eval_error)
	| EMul (b, c) -> VInt (match (eval b) with
							| VInt d -> (match (eval c) with 
										| VInt e -> d * e
										| VBool e -> raise Eval_error)
							| VBool d -> raise Eval_error)
	| EDiv (b, c) -> VInt (match (eval b) with
							| VInt d -> (match (eval c) with 
										| VInt e -> d - e
										| VBool e -> raise Eval_error)
							| VBool d -> raise Eval_error)
	| EConstBool b -> VBool b
	| EEqual (b, c) -> VBool (match (eval b) with
							| VInt d -> (match (eval c) with 
										| VInt e -> if d = e then true else false
										| VBool e -> raise Eval_error)
							| VBool d -> raise Eval_error)
	| ELessThan (b, c) -> VBool (match (eval b) with
							| VInt d -> (match (eval c) with
										| VInt e -> if d < e then true else false
										| VBool e -> raise Eval_error)
							| VBool d -> raise Eval_error)
	| EIfThenElse (b, c, d) -> match (eval b) with
								| VBool f -> if f then (eval c) else (eval d)
								| VInt f -> raise Eval_error;;
