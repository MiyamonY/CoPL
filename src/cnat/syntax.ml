open Printf;;

(* nat *)
type nat =
  | Z
  | S of nat;;

let rec pp_nat  = function
  | Z -> "Z"
  | S n ->
	sprintf "S(%s)" (pp_nat n);;

type rel =
  | Less of nat * nat;;

let pp_rel  = function
  | Less (n1, n2) ->
	sprintf "%s is less than %s" (pp_nat n1) (pp_nat n2);;
	

