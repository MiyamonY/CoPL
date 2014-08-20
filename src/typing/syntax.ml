open Printf;;
	
type op =
  | Plus
  | Minus
  | Times
  | Lt
;;

let rec pp_op_eval = function
  | Plus -> "+"
  | Minus -> "-"
  | Times -> "*"
  | Lt -> "<"
;;

let rec pp_op_is = function
  | Plus -> "plus"
  | Minus -> "minus"
  | Times -> "times"
  | Lt -> "less than"
;;
  
type var = string;;
  
let pp_var v = v;;

type tyvar = int;;
  
type types =
  | Bool
  | Int
  | Arrow of types * types
  | List of types
  | TyVar of tyvar
;;

let rec pp_types = function
  | Bool -> "bool"
  | Int -> "int"
  | Arrow (t1, t2) ->
     sprintf "(%s -> %s)" (pp_types t1) (pp_types t2)
  | List t ->
     sprintf "%s list" (pp_types t)
  | TyVar n ->
     sprintf "(Var %d)" n
;;
  
type value =
  | I of int
  | B of bool
                      
and exp =
  | Var of var
  | Val of value
  | BinOp of op * exp * exp
  | If of exp * exp * exp
  | Let of var * exp * exp
  | Fun of var * exp
  | App of exp * exp
  | RecFun of var * var * exp * exp
  | Nil
  | Cons of exp * exp
  | Match of exp * exp * var * var * exp
    
and env = (var * types) list ;;

let rec pp_value = function
  | I i -> string_of_int i
  | B b -> string_of_bool b
             
and pp_exp = function
  | Var v -> pp_var v
  | Val v -> pp_value v
  | BinOp(op, e1, e2) ->
	let str =
	  match op with
	  | Plus | Minus -> sprintf "%s %s %s"
	  | Lt -> sprintf "%s %s %s"
	  | Times ->
		begin
		  match e1, e2 with
		  | BinOp _, BinOp _ ->
			 sprintf "(%s) %s (%s)"
		  | BinOp _, _ ->
			 sprintf "(%s) %s %s"
		  | _, BinOp _ ->
			 sprintf "(%s) %s %s"
		  | _, _ -> sprintf "%s %s %s"
		end
	in
	str (pp_exp e1) (pp_op_eval op) (pp_exp e2)
  | If (e1, e2, e3) ->
	sprintf "(if %s then %s else %s)"
	  (pp_exp e1) (pp_exp e2) (pp_exp e3)
  | Let (var, e1, e2) ->
     sprintf "let %s = %s in %s"
             (pp_var var) (pp_exp e1) (pp_exp e2)
  | Fun (v, e) ->
     sprintf "(fun %s -> %s)" (pp_var v) (pp_exp e)
  | App (e1, e2) ->
     sprintf "%s (%s)" (pp_exp e1) (pp_exp e2)
  | RecFun (v1, v2, e1, e2) ->
     sprintf "let rec %s = fun %s -> %s in %s"
             (pp_var v1) (pp_var v2) (pp_exp e1) (pp_exp e2)
  | Nil -> "[]"
  | Cons (e1, e2) ->
     sprintf "(%s) :: %s" (pp_exp e1) (pp_exp e2)
  | Match(e1, e2, x, y, e3) ->
     sprintf "match %s with [] -> %s | %s :: %s -> %s"
             (pp_exp e1) (pp_exp e2) (pp_var x) (pp_var y) (pp_exp e3)
             
and pp_env env =
  let pp_pair (var, ty) =     
    sprintf "%s : %s" (pp_var var) (pp_types ty)
  in
  String.concat ", " (List.map pp_pair (List.rev env));;
      
type rel =
  | Types of env * exp * types
;;

let  pp_rel  = function
  | Types (env, e, v) ->
	   sprintf "%s |- %s : %s"
	           (pp_env env) (pp_exp e) (pp_types v)
;;

type subst = (tyvar * types) list;;
  
let rec pp_subst subst =
  let pp_subst_aux (ty1, ty2) =
    sprintf "(%s : %s)" (pp_types ty1) (pp_types ty2)
  in
  String.concat "," (List.map pp_subst_aux subst)
;;
