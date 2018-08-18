open Syntax

exception Unbound

let empty_env = []
let extend x v env = (x, v) :: env

let rec lookup x env =
	try List.assoc x env with Not_found -> raise Unbound

exception EvalErr

let rec eval_expr env e =
	match e with
	| EConstInt i ->
		VInt i
	| EConstBool b ->
		VBool b
    | EUnknownToken -> (print_string "Unknown Token...\n" ; raise EvalErr)
	| EVar x ->
		(try
			 lookup x env
		 with
         | Unbound -> (print_string "Unbound Variable: "; print_name x; print_string "\n" ; raise EvalErr))
	| EAdd (e1,e2) ->
		let v1 = eval_expr env e1 in
		let v2 = eval_expr env e2 in
		(match v1, v2 with
		 | VInt i1, VInt i2 -> VInt (i1 + i2)
		 | _ -> (print_string "Evaluation Error in EAdd...\n" ; raise EvalErr))
	| ESub (e1,e2) ->
		let v1 = eval_expr env e1 in
		let v2 = eval_expr env e2 in
		(match v1, v2 with
		 | VInt i1, VInt i2 -> VInt (i1 - i2)
		 | _ -> (print_string "Evaluation Error in ESub...\n" ; raise EvalErr))
	| EMul (e1,e2) ->
		let v1 = eval_expr env e1 in
		let v2 = eval_expr env e2 in
		(match v1, v2 with
		 | VInt i1, VInt i2 -> VInt (i1 * i2)
		 | _ -> (print_string "Evaluation Error in EMul...\n" ; raise EvalErr))
	| EDiv (e1,e2) ->
		let v1 = eval_expr env e1 in
		let v2 = eval_expr env e2 in
		(match v1, v2 with
		 | VInt i1, VInt i2 -> VInt (i1 / i2)
		 | _ -> (print_string "Evaluation Error in EDiv...\n" ; raise EvalErr))
	| EAnd (e1,e2) ->
		let v1 = eval_expr env e1 in
		let v2 = eval_expr env e2 in
		(match v1, v2 with
		 | VBool i1, VBool i2 -> VBool (i1 && i2)
		 | _ -> (print_string "Evaluation Error in EAnd...\n" ; raise EvalErr))
	| EOr (e1,e2) ->
		let v1 = eval_expr env e1 in
		let v2 = eval_expr env e2 in
		(match v1, v2 with
		 | VBool i1, VBool i2 -> VBool (i1 || i2)
		 | _ -> (print_string "Evaluation Error in EOr...\n" ; raise EvalErr))
    | EEq (e1,e2) ->
		let v1 = eval_expr env e1 in
		let v2 = eval_expr env e2 in
		(match v1, v2 with
		 | VInt i1,	VInt i2	-> VBool (i1 = i2)
		 | _ -> (print_string "Evaluation Error in EEq...\n" ; raise EvalErr))
	| ELt (e1,e2) ->
		let v1 = eval_expr env e1 in
		let v2 = eval_expr env e2 in
		(match v1, v2 with
		 | VInt i1,	VInt i2	-> VBool (i1 < i2)
		 | _ -> (print_string "Evaluation Error in ELt...\n" ; raise EvalErr))
	| EIf (e1,e2,e3) ->
		let v1 = eval_expr env e1 in
		(match v1 with
		 | VBool b ->
			 if b then eval_expr env e2 else eval_expr env e3
		 | _ -> (print_string "Evaluation Error in EIf...\n" ; raise EvalErr))
    | ELet (e1,e2,e3) ->
        let new_env = (e1, eval_expr env e2)::(env) in
        eval_expr new_env e3
    | EFun (x,e) -> VFun (x,e,ref env)
    | ELetRec (f,x,e1,e2) ->
        let oenv = ref [] in
        let v = VFun (x,e1,oenv) in
            (oenv := extend f v env;
             eval_expr (extend f v env) e2)
    | EApp (e1,e2) ->
        let v1 = eval_expr env e1 in
        let v2 = eval_expr env e2 in
        let rec linkenv env1 env2 = env1@env2 in
        (match v1 with
         | VFun(x,e,oenv) -> (oenv := linkenv env !oenv; (*print_env !oenv;*) eval_expr (extend x v2 !oenv) e)
         | _ -> (print_string "Evaluation Error in EApp...\n" ; raise EvalErr))
 
let rec eval_command env c =
	match c with
    | CExp e -> [("-", env, eval_expr env e )]
    | CDecl (e1,e2) -> [("-", (e1, (eval_expr env e2))::env, eval_expr env e2)]
    | CDeclLet (e1,e2,e3) -> [("-", (e1, (eval_expr env e2))::env, eval_expr ((e1, (eval_expr env e2))::env) e2)]@(eval_command ((e1, (eval_expr env e2))::env) e3)
    | CDeclare e -> eval_command env e
    | CDeclAnd (e1,e2,e3) -> [("-", (e1, (eval_expr env e2))::env, eval_expr env e2)]@(eval_command env e3)
    | CDeclAL (e1,e2,e3) -> [("-", (e1, (eval_expr env e2))::env, eval_expr env e2)]@(eval_command ((e1, (eval_expr env e2))::env) e3)
    | CLetRec (f, x, e, e2) -> [("-", (f, eval_expr env (ELetRec (f,x,e,e2)))::env, eval_expr env (ELetRec (f,x,e,e2)))]
    | CRecDecl (f,x,e) ->
        let oenv = ref [] in
        let v = VFun (x, e, oenv) in
        (oenv := extend f v env; [("-", !oenv, VFun (x, e, ref []))])
    | CRecAnd (f,x,e,next_rec) ->
        let oenv = ref [] in
        let v = VFun (x, e, oenv) in
        (oenv := extend f v env;
         [("-", !oenv, VFun (x, e, ref []))]@(eval_command (!oenv) next_rec))
    | CExit -> raise QUIT


