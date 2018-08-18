type tyvar = int

exception TyError

type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar
  | TyPair of ty * ty
  (*| TyNil*)
  | TyCons of ty

(*
 * Generate a fresh type variable
 *   (i.e. a type variable that has not been appeared)
 *)

type constraints = (ty * ty) list
type subst = (tyvar * ty) list

let counter = ref 0

let new_tyvar () =
    (counter := !counter + 1; !counter)

(* val print_type : ty -> unit *)
let rec print_type ty =
    match ty with
    | TyInt -> print_string "int"
    | TyBool -> print_string "bool"
    | TyFun (ty1, ty2) ->
            (print_string "(";
             print_type ty1;
             print_string " -> ";
             print_type ty2;
             print_string ")")
    | TyVar tyvar -> print_string (string_of_int tyvar)
    | TyPair (ty1, ty2)-> 
            (print_string "(";
             print_type ty1;
             print_string " * ";
             print_type ty2;
             print_string ")")
    (*| TyNil -> print_string "[Nil]"*)
    | TyCons a ->
            (print_type a;
             print_string " list")

let rec print_constr constr =
    match constr with
    | [] -> print_string "end_constr\n"
    | (ty1, ty2)::yd -> 
            (print_string "(";
             print_type ty1;
             print_string ", ";
             print_type ty2;
             print_string ")";
             print_constr yd)

let rec print_subst subst =
    match subst with
    | [] -> print_string "end_subst\n"
    | (tyvar, ty)::yd -> 
            (print_string "(";
             print_string (string_of_int tyvar);
             print_string ", ";
             print_type ty;
             print_string ")";
             print_subst yd)
