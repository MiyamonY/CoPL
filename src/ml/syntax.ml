open Printf;;

type value =
  | I of int
  | B of bool
;;

let rec pp_value = function
  | I i -> string_of_int i
  | B b -> string_of_bool b
;;
	
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

type exp =
  | Val of value
  | BinOp of op * exp * exp
  | If of exp * exp * exp
;;

let rec pp_exp = function
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
;;

type rel =
  | EvalTo of exp * value
  | Is of op * value * value * value
;;

let  pp_rel  = function
  | EvalTo (e, v) ->
	sprintf "%s evalto %s"
	(pp_exp e) (pp_value v)
  | Is (op, n1, n2, n) ->
	sprintf "%s %s %s is %s"
	  (pp_value n1) (pp_op_is op) (pp_value n2) (pp_value n)
;;

