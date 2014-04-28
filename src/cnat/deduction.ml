open Format;;
open Rule;;
open Lib;;
open Syntax;;

exception DeductionErrro of string;;

let deduction_less n1 n2 =
  if S n1 = n2 then (LSucc, [])
  else
	(LTrans, [Less(n1, S n1);
			  Less(S n1, n2)]);;

let rec deduction rel =
  match rel with
  | Less(n1, n2) ->
	let r, dtree = deduction_less n1 n2 in
	Tr((rel, r), List.map deduction dtree)
;;

let rec pp_dtree buf n = function
  | Tr ((rel, r), tree) ->
	fprintf buf "%s by %s " (pp_rel rel) (pp_rule r);
	begin
	  match tree with
	  | [] ->
		fprintf buf "{ };@,"
	  | _ ->
		fprintf buf "{@[<v 1>@,";
		List.iter (pp_dtree buf n) tree;
		fprintf buf "@]};@,@]";
	end;;

let pp_dtree_top buf = function
  | Tr ((rel, r), tree) ->
	fprintf buf "%s by %s {@[<v 1>@," (pp_rel rel) (pp_rule r);
	List.iter (pp_dtree buf 1) tree;
	fprintf buf "@]}@.";;
