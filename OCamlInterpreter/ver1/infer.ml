open Syntax
open TySyntax
open ConstraintSolver

type tyenv = (name * ty) list

let empty_tyenv = []

let extend x v tyenv = (x, v) :: tyenv

let rec lookup x tyenv =
	try List.assoc x tyenv with Not_found -> raise Unbound

(* infer_expr : tyenv -> expr -> ty * constraints *)
let rec infer_expr tyenv expr =
	match expr with
	| EConstInt i ->
        (TyInt, [])
	| EConstBool b ->
        (TyBool, [])
    (*| EUnknownToken -> (print_string "Unknown Token...\n" ; raise EvalErr)*)
	| EVar x ->
		(try
            ((lookup x tyenv), [])
		 with
         | Unbound -> (print_string "Unbound Variable: "; print_name x; print_string "\n" ; raise EvalErr))
	| EAdd (e1, e2) | ESub (e1, e2) | EMul (e1, e2) | EDiv (e1, e2) -> 
		(let (ty1, constr1) = infer_expr tyenv e1 in
		 let (ty2, constr2) = infer_expr tyenv e2 in
		 (TyInt, [(ty1, TyInt); (ty2, TyInt)]@constr1@constr2))
(*
    | EAnd (e1, e2) | EOr (e1, e2) ->
		(let (ty1, constr1) = infer_expr tyenv e1 in
		 let (ty2, constr2) = infer_expr tyenv e2 in
         (TyBool, [(ty1, TyBool); (ty2, TyBool)]@constr1@constr2))
*)
    | EEq (e1, e2) | ELt (e1, e2) ->
		(let (ty1, constr1) = infer_expr tyenv e1 in
		 let (ty2, constr2) = infer_expr tyenv e2 in
         (TyBool, [(ty1, ty2)]@constr1@constr2))
	| EIf (e1, e2, e3) ->
		let (ty1, constr1) = infer_expr tyenv e1 in
		let (ty2, constr2) = infer_expr tyenv e2 in
		let (ty3, constr3) = infer_expr tyenv e3 in
        (ty2, [(ty1, TyBool)]@[(ty2, ty3)]@constr1@constr2@constr3)
    | ELet (x,e1,e2) ->
        let (ty1, constr1) = infer_expr tyenv e1 in
        let (ty2, constr2) = infer_expr (extend x ty1 tyenv) e2 in
        (ty2, constr1@constr2)
    | EFun (x, e) ->
        let a = TyVar (new_tyvar ()) in
        let (ty, constr) = infer_expr (extend x a tyenv) e in
        (TyFun (a, ty), constr)
    | ELetRec (f,x,e1,e2) ->
        let a = TyVar (new_tyvar ()) in
        let b = TyVar (new_tyvar ()) in
        let (ty1, constr1) = infer_expr (extend f (TyFun (a, b)) (extend x a tyenv)) e1 in
        let (ty2, constr2) = infer_expr (extend f (TyFun (a, b)) tyenv) e2 in
        (ty2, [(ty1, b)]@constr1@constr2)
    | EApp (f,x) ->
        let (ty1, constr1) = infer_expr tyenv f in
        let (ty2, constr2) = infer_expr tyenv x in
        let a = TyVar (new_tyvar ()) in
        (a, [(ty1, TyFun (ty2, a))]@constr1@constr2)
    | EPair (y, yd) -> 
        let (ty1, constr1) = infer_expr tyenv y in
        let (ty2, constr2) = infer_expr tyenv yd in    
        (TyPair (ty1, ty2), constr1@constr2)
    | ENil ->
        let a = TyVar (new_tyvar ()) in
        (a, [])
    | ECons (y, yd) -> 
        let (ty1, constr1) = infer_expr tyenv y in
        let (ty2, constr2) = infer_expr tyenv yd in    
        let a = TyVar (new_tyvar ()) in
        (TyCons a, [(a, ty1); (TyCons a, ty2)]@constr1@constr2)
    | EMatch (obj, cases) -> 
        let (ty, constr) = infer_expr tyenv obj in
        let a = TyVar (new_tyvar ()) in
        (* match_expr : tyenv -> (ty * ty) list -> (ty * (ty * ty) list) *)
        let rec match_expr match_tyenv match_cases =
            match match_cases with
            | [] -> (a, [])
            | (p, e)::yd -> 
                let rec match_pattern pattern =
                    match pattern with
                    | PInt i -> (TyInt, [], [])
                    | PBool b -> (TyBool, [], [])
                    | PVar name -> 
                        let c = TyVar (new_tyvar ()) in (c, [], [(name, c)])
                    | PPair (p1, p2) -> 
                        let (typ1, constrp1, tyenvp1) = match_pattern p1 in
                        let (typ2, constrp2, tyenvp2) = match_pattern p2 in
                        (TyPair (typ1, typ2), constrp1@constrp2, tyenvp1@tyenvp2)
                    | PNil -> 
                        let c = TyVar (new_tyvar ()) in (c, [], [])
                    | PCons (p1, p2) -> 
                        let (typ1, constrp1, tyenvp1) = match_pattern p1 in
                        let (typ2, constrp2, tyenvp2) = match_pattern p2 in
                        let c = TyVar (new_tyvar ()) in
                        (TyCons c, [(c, typ1); (TyCons c, typ2)]@constrp1@constrp2, tyenvp1@tyenvp2)
                in
                let (typ, constrp, tyenvp) = match_pattern p in
                let (tye, constre) = infer_expr (tyenvp@match_tyenv) e in
                let (tyyd, constryd) = match_expr match_tyenv yd in
                (a, [(ty, typ); (a, tye)]@constrp@constre@constryd)
        in match_expr tyenv cases

(* infer_cmd : tyenv -> cmd -> ty * tyenv *)
let rec infer_cmd tyenv cmd =
    match cmd with
    | CExp e -> (
        let (ty, constr) = infer_expr tyenv e in
        let te = ty_subst (ty_unify constr) ty in
        (te, tyenv))
    | CDecl (x, e) -> (
        let (ty, constr) = infer_expr tyenv e in
        let tx = ty_subst (ty_unify constr) ty in
        (tx, (extend x tx tyenv)))
(*
    | CDeclLet (x, e, next_cmd) -> raise TyError
    | CDeclare e -> infer_cmd tyenv e
    | CDeclAnd (x, e, next_cmd) -> raise TyError
    | CDeclAL (x, e, next_cmd) -> raise TyError
    | CLetRec (f, x, e, e2) -> raise TyError
*)
    | CRecDecl (f, x, e) -> (
        let a = TyVar (new_tyvar ()) in
        let b = TyVar (new_tyvar ()) in
        let (ty, constr) = infer_expr (extend f (TyFun (a, b)) (extend x a tyenv)) e in
        let ft = ty_subst (ty_unify ((ty, b)::constr)) (TyFun (a, b)) in
        (ft, (extend f ft tyenv)))
(*  
    | CRecAnd (f, x, e, next_rec) -> raise TyError
    | CExit -> raise QUIT
*)



(*
    with TyError -> raise (InferErr ("Infer Error :" ^ str))


let rec infer_cmd tyenv cmd =
	match cmd with
    | CExp e -> [("-", env, eval_expr env e)]
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
*)

