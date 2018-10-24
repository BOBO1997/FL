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
