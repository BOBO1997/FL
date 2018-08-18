open Syntax

let empty_env = []

(* env is not ref *)
let extend x thunk env = (x, thunk) :: env

(* env is not ref *)
let rec lookup x (env: env) =
	try List.assoc x env with Not_found -> raise Unbound

let rec eval_expr (env : env) e =
	match e with
	| EConstInt i ->
		VInt i
	| EConstBool b ->
		VBool b
    (*| EUnknownToken -> (print_string "Unknown Token...\n" ; raise EvalErr)*)
	| EVar x ->
		(try
            let (dval : dval ref) = lookup x env in
            (match !dval with
            | DThunk (expr, name_env) ->
                let v = eval_expr !name_env expr in
                dval := DVal v; v
            | DVal v -> v)
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
(*
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
*)
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
    | ELet (name, expr1, expr2) ->
        let new_env = (name, ref (DThunk (expr1, ref env)))::env in
        eval_expr new_env expr2
    | EFun (x,e) -> VFun (x,e,ref env)
    | ELetRec (f,x,e1,e2) ->
        let oenv = ref [] in
        let efun = EFun (x, e1) in
            (oenv := extend f (ref (DThunk (efun, ref env))) env;
             eval_expr !oenv e2)
    | EApp (f, e) ->
        let v1 = eval_expr env f in
        let thunk = ref (DThunk (e, ref env)) in
        (match v1 with
         | VFun (x, e_in_fun, oenv) -> eval_expr (extend x thunk !oenv) e_in_fun
         | _ -> (print_string "Evaluation Error in EApp...\n" ; raise EvalErr))
    | EPair (y, yd) -> VPair (eval_expr env y, eval_expr env yd)
    | ENil -> VNil
    | ECons (y, yd) -> VCons (eval_expr env y, eval_expr env yd)
    | EMatch (obj, cases) -> find_list cases env obj

(* find_match : pattern -> env -> expr -> (name * thunk) list option *)
and find_match p env obj =
    match (p, obj) with
    | (PInt px, EConstInt vx) -> if px = vx then Some [] else None
    | (PBool px, EConstBool vx) -> if px = vx then Some [] else None
    | (PVar px, _) -> Some [(px, ref (DThunk (obj, ref env)))]
    | (PPair (px1, px2), EPair (e1, e2)) ->
            (match (find_match px1 env e1, find_match px2 env e2) with
            | (Some list1, Some list2) -> Some (list1@list2)
            | (_, _) -> None)
    | (PNil, ENil) -> Some []
    | (PCons (px1, px2), ECons (e1, e2)) ->
            (match (find_match px1 env e1, find_match px2 env e2) with
            | (Some list1, Some list2) -> Some (list1@list2)
            | (_, _) -> None)
    | (_, _) -> None

(* find_list : pattern list -> env -> expr -> value *)
and find_list cases env obj =
    match cases with
    | [] -> print_string "in find_list, do not match any patterns...\n"; raise MatchErr
    | (p, expr)::yd -> (match find_match p env obj with
                        | None -> find_list yd env obj
                        | Some pattern -> eval_expr (pattern@env) expr)


let rec eval_command env c =
	match c with
    | CExp e -> ("-", env, eval_expr env e )
    | CDecl (x,expr) -> 
        let thunk = ref (DThunk (expr, ref env)) in
        let new_env = extend x thunk env in
        ("-", new_env, eval_expr new_env expr)
    | CRecDecl (f,x,e) ->
        let oenv = ref [] in
        let thunk = ref (DThunk (EFun (x, e), oenv)) in
        (*let v = VFun (x, e, oenv) in*)
        (oenv := extend f thunk env; ("-", !oenv, VFun (x, e, ref [])))
        (*(oenv := extend f thunk env; ("-", extend f thunk env, VFun (x, e, ref [])))
*)
