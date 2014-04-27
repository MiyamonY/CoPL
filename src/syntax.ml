open Printf;;

(* syntax *)
type op =
  | Plus
  | Times;;

let  pp_op  = function
  | Plus -> "plus"
  | Times -> "times";;

type nat =
  | Z
  | S of nat;;

let rec pp_term = function
  | Z -> "Z"
  | S n ->
	sprintf "S(%s)" (pp_term n);;

type exp =
  | BinOp of op * exp * exp
  | Val of nat ;;

let rec pp_exp  = function
  | BinOp (op, e1, e2)->
	sprintf "%s %s %s"
	  (pp_exp e1) (pp_op op) (pp_exp e2)
  | Val t ->
	(pp_term t);;

type rel =
  | Is of exp * nat;;

let rec pp_rel  = function
  | Is (e, t) ->
	sprintf "%s is %s" (pp_exp e) (pp_term t);;

