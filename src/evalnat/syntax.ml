open Printf;;

type nat =
  | Z
  | S of nat;;

let rec pp_nat = function
  | Z -> "Z"
  | S n ->
	sprintf "S(%s)" (pp_nat n);;

type op =
  | Plus
  | Times;;

let rec pp_op_eval = function
  | Plus -> "+"
  | Times -> "*";;

let rec pp_op_is = function
  | Plus -> "plus"
  | Times -> "times";;

type exp =
  | Val of nat
  | BinOp of op * exp * exp;;

let rec pp_exp = function
  | Val n -> pp_nat n
  | BinOp(op, e1, e2) ->
	let str =
	  match op with
	  | Plus -> sprintf "%s %s %s"
	  | Times ->
		begin
		  match e1, e2 with
		  | BinOp (Plus, _, _), BinOp (Plus, _, _) -> sprintf "(%s) %s (%s)"
		  | BinOp (Plus, _, _), _ -> sprintf "(%s) %s %s"
		  | _, BinOp(Plus, _, _) -> sprintf "%s %s (%s)"
		  | _, _ -> sprintf "%s %s %s"
		end
	in
	str (pp_exp e1) (pp_op_eval op) (pp_exp e2);;

type rel =
  | EvalTo of exp * nat
  | Is of op * nat * nat * nat;;

let  pp_rel  = function
  | EvalTo(e, n) ->
	sprintf "%s evalto %s"
	  (pp_exp e) (pp_nat n)
  | Is (op, n1, n2, n) ->
	sprintf "%s %s %s is %s"
	  (pp_nat n1) (pp_op_is op) (pp_nat n2) (pp_nat n);;


