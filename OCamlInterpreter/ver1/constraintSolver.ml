open TySyntax

let rec lookupVar subst tyvar =
	match subst with
	| [] -> TyVar tyvar
	| (tyVar, t)::yd-> if tyvar = tyVar then t else (lookupVar yd tyvar)

(* ty_subst : subst -> ty -> ty *)
let rec ty_subst subst ty =
	match ty with
	| TyInt -> TyInt
	| TyBool -> TyBool
	| TyFun (ty1, ty2) -> TyFun (ty_subst subst ty1, ty_subst subst ty2)
	| TyVar tyvar -> lookupVar subst tyvar
    | TyPair (ty1, ty2) -> TyPair (ty_subst subst ty1, ty_subst subst ty2)
    | TyCons tyList -> ty_subst subst tyList

(* subst_ty : ty -> ty -> ty -> ty 
 * 与えられたtyの中での古いtyを新しいtyに置換したtyを返す *)
let rec subst_ty old_ty new_ty ty =
	match ty with
	| x  when x = old_ty -> new_ty
	| TyFun (ty1, ty2) -> TyFun (subst_ty old_ty new_ty ty1, subst_ty old_ty new_ty ty2)
	| _ -> ty

(* subst_subst : ty -> ty -> subst -> subst 
 * 与えられたsubst : (tyvar, ty) list の中で、
 * 古いtyを新しいtyに置換したsubstを返す *)
let rec subst_subst old_ty new_ty subst =
	match subst with
	| [] -> []
	| (ty1, ty2)::yd -> ((ty1, subst_ty old_ty new_ty ty2)::(subst_subst old_ty new_ty yd))

(* type subst = (tyvar * ty) list
 * compose : subst -> subst -> subst
 * (σ ∘ σ′) α = σ(σ′(α))
 * [α ≔  t] β = β (if α ≠ β)
 *)
let rec compose subst_f subst_g =
	match subst_g with
	| [] -> subst_f
	| (tyvar,ty)::subst -> 
        (match ty with
		| TyInt | TyBool | TyCons _ | TyFun (_, _) | TyPair (_, _) -> 
                (tyvar, ty)::(compose (subst_subst (TyVar tyvar) ty subst_f) (subst_subst (TyVar tyvar) ty subst))
		| TyVar tyvarf -> (tyvar, (subst_ty (TyVar tyvar) ty (lookupVar subst_f tyvarf)))
                            ::(compose (subst_subst (TyVar tyvar) ty subst_f) subst)
        (*| TyCons a -> *))

(* occurence_detect : int -> ty -> ty *)
(* 単一化の失敗ケースを検出 *)
let rec occurence_detect a ty =
	match ty with
	| TyVar i when i = a -> raise TyError
	| TyFun (ty1, ty2) -> TyFun (occurence_detect a ty1, occurence_detect a ty2)
	| _ -> ty

(* substitute : ty -> ty -> (ty * ty) list -> (ty * ty) list *)
let rec substitute old_ty new_ty constraints =
	match constraints with
	| [] -> []
	| (ty1, ty2)::yd -> ((subst_ty old_ty new_ty ty1, subst_ty old_ty new_ty ty2)::(substitute old_ty new_ty yd))

(* ty_unify : (ty * ty) list -> subst
 * unify {} = []
 * unify ({ s=s }∪ c) = unify(C)
 * unify ({ s→ t = s'→ t' }∪ c) = unify ({ s=s', t=t' }∪ c)
 * unify ({ α=t }∪ c) = (unify (C [α:= t])) ∘ [α:=t]
 * unify ({ t=α }∪ c) = (unify (C [α:= t])) ∘ [α:=t]
 * unify ({ s*t = s'*t' }∪ C) = unify ({ s=s', t=t' }∪ C)
 * unify ({ t list = t' list }∪ C) = unify ({ t=t' }∪ C)
 *)
let rec ty_unify constraints =
	match constraints with
	| [] -> []
	| (ty1, ty2)::yd -> 
        (match (ty1, ty2) with
		| (TyInt, TyInt) | (TyBool, TyBool) -> ty_unify yd
		| (TyFun (s1, t1), TyFun(s2, t2)) | (TyPair (s1, t1), TyPair (s2, t2)) -> ty_unify ((s1, s2)::(t1, t2)::yd)
		| (ty, TyVar tyvar) | (TyVar tyvar, ty) -> 
				if (TyVar tyvar) = ty then ty_unify yd
                else
				    let soundty = occurence_detect tyvar ty in 
                    compose [(tyvar, soundty)] (ty_unify (substitute (TyVar tyvar) soundty yd))
        | (TyCons a1, TyCons a2) -> ty_unify ((a1, a2)::yd)
        | (_, _) -> print_string "in ty_unify, do not match any patterns...\n"; raise TyError)

