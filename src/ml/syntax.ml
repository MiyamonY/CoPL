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

type value =
  | I of int
  | B of bool
  | F of env * var * exp
  | RF of env * var * var * exp
    
and exp =
  | Var of var
  | Val of value
  | BinOp of op * exp * exp
  | If of exp * exp * exp
  | Let of var * exp * exp
  | Fun of var * exp
  | App of exp * exp
  | RecFun of var * var * exp * exp
      
and env = (var * value) list ;;

let rec pp_value = function
  | I i -> string_of_int i
  | B b -> string_of_bool b
  | F (env, v, e) ->
     sprintf "(%s) [fun %s -> %s]"
             (pp_env env) (pp_var v) (pp_exp e)
  | RF (env, v1, v2, e) ->
     sprintf "(%s)[rec %s = fun %s -> %s]"
             (pp_env env) (pp_var v1) (pp_var v2) (pp_exp e)
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
             
and pp_env env =
  let pp_pair (var,value) =     
    sprintf "%s = %s" (pp_var var) (pp_value value)
  in
  String.concat ", " (List.map pp_pair (List.rev env));;
      
type rel =
  | EvalTo of env * exp * value
  | Is of op * value * value * value
;;

let  pp_rel  = function
  | EvalTo (env, e, v) ->
	   sprintf "%s |- %s evalto %s"
	           (pp_env env) (pp_exp e) (pp_value v)
  | Is (op, n1, n2, n) ->
	   sprintf "%s %s %s is %s"
	           (pp_value n1) (pp_op_is op) (pp_value n2) (pp_value n)
;;

