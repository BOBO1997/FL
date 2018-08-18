exception QUIT

type name = string 

type value =
	| VInt	of int
	| VBool of bool
    | VFun of name * expr * env ref
and env = (name * value) list
and expr =
	| EConstInt	of int
	| EConstBool of bool
	| EVar			 of name 
	| EAdd			 of expr * expr
	| ESub			 of expr * expr
	| EMul			 of expr * expr
	| EDiv			 of expr * expr
    | EAnd           of expr * expr
    | EOr            of expr * expr
    | EEq			 of expr * expr
	| ELt			 of expr * expr
	| EIf			 of expr * expr * expr
	| ELet			 of name * expr * expr
    | EFun           of name * expr
    | ELetRec        of name * name * expr * expr
    (*| ELetRec        of (name * name * expr) list * expr*)
    | EApp           of expr * expr
    | EUnknownToken

type command =
	| CExp	of expr
    | CDecl of name * expr
	| CDeclLet of name * expr * command
    | CDeclare of command
    | CDeclAnd of name * expr * command
    | CDeclAL of name * expr * command
    | CRecDecl of name * name * expr
    (*| CRecDecl of (name * name * expr) list*)
    | CLetRec of name * name * expr * expr
    (*| CLetRec of (name * name * expr) list * expr*)
    | CRecAnd of name * name * expr * command
    | CExit

let print_name = print_string 

let rec print_value v =
	match v with
	| VInt i	-> print_int i
	| VBool b -> print_string (string_of_bool b)
    | VFun (x,e,env) -> print_string "<fun>"
    (*| VFun (x,e,oenv) -> (print_string "(";
                         print_name x;
                         print_string ", ";
                         print_expr e;
                         print_string ", ";
                         print_env !oenv;
                         print_string ") ")
*)


(*
 小さい式に対しては以下でも問題はないが，
 大きいサイズの式を見やすく表示したければ，Formatモジュール
	 http://caml.inria.fr/pub/docs/manual-ocaml/libref/Format.html
 を活用すること
*)
and print_expr e =
	match e with
	| EConstInt i ->
		 print_int i
	| EConstBool b ->
		 print_string (string_of_bool b)
	| EVar x -> 
		 print_name x
	| EAdd (e1,e2) -> 
		   (print_string "EAdd (";
			print_expr e1;
			print_string ",";
			print_expr e2;
			print_string ")")
	| ESub (e1,e2) -> 
		   (print_string "ESub (";
			print_expr e1;
			print_string ",";
			print_expr e2;
			print_string ")")
	| EMul (e1,e2) -> 
		   (print_string "EMul (";
			print_expr e1;
			print_string ",";
			print_expr e2;
			print_string ")")
	| EDiv (e1,e2) -> 
		   (print_string "EDiv (";
			print_expr e1;
			print_string ",";
			print_expr e2;
			print_string ")")
	| EAnd (e1,e2) -> 
		   (print_string "EAnd (";
			print_expr e1;
			print_string ",";
			print_expr e2;
			print_string ")")
	| EOr (e1,e2) -> 
		   (print_string "EOr (";
			print_expr e1;
			print_string ",";
			print_expr e2;
			print_string ")")
    | EEq (e1,e2) ->
		   (print_string "EEq (";
			print_expr e1;
			print_string ",";
			print_expr e2;
			print_string ")")
	| ELt (e1, e2) ->
		   (print_string "ELt (";
			print_expr e1;
			print_string ",";
			print_expr e2;
			print_string ")")
	| EIf (e1,e2,e3) ->
		   (print_string "EIf (";
			print_expr	 e1;
			print_string ","; 
			print_expr	 e2;
			print_string ",";
			print_expr	 e3;
			print_string ")")
    | ELet (e1,e2,e3) ->
           (print_string "ELet (";
            print_name   e1;
            print_string ",";
            print_expr   e2;
            print_string ",";
            print_expr   e3;
            print_string ")")
    | EFun (x,e) ->
           (print_string ("EFun (" ^ x ^ ",");
            print_expr e;
            print_string ")")
    | EApp (e1,e2) ->
           (print_string "EApp (";
            print_expr e1;
            print_string ",";
            print_expr e2;
            print_string ")")
    | ELetRec (id,x,e1,e2) ->
           (print_string ("ELetRec (" ^ id ^ "," ^ x ^ ",");
            print_expr e1;
            print_string ",";
            print_expr e2;
            print_string ")")
 
    (*| ELetRec (decls,e) ->
           (print_string ("ELetRec ([");
             List.iter (fun (id,x,e) ->
		        print_string ("(" ^ id ^ "," ^ x ^ ",");
                print_expr e;
		        print_string ");")
		    decls;
            print_string "],";
            print_expr e;
            print_string ")")*)
    | EUnknownToken -> ()

and print_env env =
    (print_string "[";
    let rec sub_print env =
    match env with
    | [] -> print_string "]"
    | (n, v)::yd -> (print_string "(";
                     print_name n;
                     print_string ",";
                     print_value v;
                     print_string ")";
                     sub_print yd)
    in sub_print env)

and print_command p =			 
	match p with
	| CExp e -> print_expr e
    | CDecl (e1,e2) -> (print_name e1; print_expr e2)
    | CDeclLet (e1,e2,e3) -> (print_name e1; print_expr e2; print_command e3)
    | CDeclare e -> print_command e
    | CDeclAnd (e1,e2,e3) -> (print_name e1; print_expr e2; print_command e3)
    | CDeclAL (e1,e2,e3) -> (print_name e1; print_expr e2; print_command e3)
    (*| CRecDecl decls ->
       (print_string ("ERecDecl ([");
          List.iter (fun (id,x,e) ->
	    	print_string ("(" ^ id ^ "," ^ x ^ ",");
		    print_expr e;
		    print_string ");")
		  decls;
        print_string "])")*)
    | CRecDecl (id,x,e) ->
        (print_string ("CRecDecl (" ^ id ^ "," ^ x ^ ",");
         print_expr e;
         print_string ")")
    | CLetRec (id,x,e,e2) ->
        (print_string ("CRecDecl (" ^ id ^ "," ^ x ^ ",");
         print_expr e;
         print_string ")")
    | CRecAnd (e1,e2,e3,e4) -> (print_name e1; print_name e2;  print_expr e3; print_command e4)
    | CExit -> ()
