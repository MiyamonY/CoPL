open Format;;
open Rule;;
open Lib;;
open Syntax;;

exception DeductionError of string;;

let deduction_less1 n1 n2 =
  if S n1 = n2 then (`LSucc, [])
  else
	(`LTrans, [Less(n1, S n1);
			  Less(S n1, n2)]);;

let  deduction_less2 n1 n2 =
  match n1 with
  | Z -> (`LZero, [])
  | S n1' ->
	match n2 with
	| Z -> raise (DeductionError "deduction_less2:n2 is Z")
	| S n2' -> (`LSuccSucc, [Less(n1', n2');]);;

let deduction_less3 n1 n2 =
  match n2 with
	| S n2' when n1 = n2' -> (`LSucc, [])
	| S n2' -> (`LSuccR, [Less(n1, n2');])
	| _ -> raise (DeductionError "deduction_less3:n2 is Z");;

let rec deduction n rel =
  let deduction_less =
	match n with
	| 1 -> deduction_less1
	| 2 -> deduction_less2
	| 3 -> deduction_less3
	| _ -> failwith ("invalid arg: " ^ (string_of_int n))
  in
  match rel with
  | Less(n1, n2) ->
	let r, dtree = deduction_less n1 n2 in
	Tr((rel, r), List.map (deduction n) dtree)
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
