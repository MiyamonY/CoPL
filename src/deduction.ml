open Syntax;;
open Rule;;
open Format;;
open Lib;;

exception DeductionError of string;;

let fun_op op = 
  let rec plus x y = match x with
	  | Z -> y
	  | S x' -> plus x' (S y) in
  let rec times x y = match x with
	| Z -> Z
	| S Z -> y
	| S x' -> plus y (times x' y)
  in
  match op with
  | Plus -> plus
  | Times -> times;;

let rec eval_exp = function
  | BinOp (op, e1, e2) ->
	(fun_op op) (eval_exp e1) (eval_exp e2)
  | Val n -> n;;
	
let rec pp_dtree buf n = function
  | Tr ((rel, r), tree) ->
	fprintf buf "%s by %s " (pp_rel rel) (pp_rule r);
	begin
	  match tree with
	  | [] ->
		fprintf buf "{ };@,"
	  | _ ->
		fprintf buf "{@[<1>@,";
		List.iter (pp_dtree buf n) tree;
		fprintf buf "@,@]};";
	end;;

let pp_dtree_top buf = function
  | Tr ((rel, r), tree) ->
	fprintf buf "%s by %s {@[<1>@ " (pp_rel rel) (pp_rule r);
	List.iter (pp_dtree buf 1) tree;
	fprintf buf "@ }@]@.";;

let deduction_step e v =
  match e with
  | BinOp(Plus, e1, e2) ->
	begin
	  match e1, v with
	  | Val Z, v ->
		if e2 = Val v then (PZero, [])
		else raise (DeductionError "deduction_step: plus")
	  | Val (S n), S v' ->
		(PSucc, [Is (BinOp(Plus, Val n, e2), v')])
	  | _ -> raise (DeductionError "deduction_step: plus")
	end
  | BinOp(Times, e1, e2) ->
	begin
	  match e1, v with
	  | Val Z, v ->
		if v = Z then (TZero, [])
		else raise (DeductionError "deduction_step: times")
	  | Val (S n1), n4 ->
		let n3 = eval_exp (BinOp (Times, Val n1, e2)) in
		(TSucc, [Is (BinOp(Times, Val n1, e2), n3);
				 Is (BinOp(Plus, e2, Val n3), n4)])
	  | _ -> raise (DeductionError "deduction_step: times")
	end
  | Val _ -> raise (DeductionError "deduction_step: val") ;;

	
let rec deduction rel =
  match rel with
  | Is(e, v) ->
	let r, rel_list = deduction_step e v in
	let ded_tree = List.map deduction rel_list in
	Tr ((rel, r), ded_tree);;

