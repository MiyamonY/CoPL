open Format;;
open Syntax;;
open Rule;;
open Lib;;

exception DeductionError of string;;

let rec nat_fun op =
  let rec plus x y =
	match x with
	| Z -> y
	| S x' -> plus x' (S y) in
  let rec times x y =
	match x with
	| Z -> Z
	| S x' -> plus y (times x' y)
  in
  match op with
  | Plus -> plus
  | Times -> times
;;

let rec eval = function
  | Val n -> n
  | BinOp (op, e1, e2) -> (nat_fun op) (eval e1) (eval e2)
;;
 
let deduction_eval_binop op e1 e2 n =
  let r = match op with
	| Plus -> EPlus
	| Times -> ETimes
  in
  let n1 = eval e1 in
  let n2 = eval e2 in
  (r, [EvalTo (e1, n1);
	   EvalTo (e2, n2);
	   Is (op, n1, n2, n)])
;;
	
let deduction_eval e n =
  match e with
  | BinOp (op, e1, e2) ->
	deduction_eval_binop op e1 e2 n
  | Val _ ->
	(EConst, [])
;;

let deduction_is op n1 n2 n =
  match op with
  | Plus ->
	begin
	  match n1, n with
	  | Z, _ ->
		if n2 = n then (PZero, [])
		else raise (DeductionError "deduction_is: plus")
	  | S n1', S n' ->
		(PSucc, [Is(Plus, n1', n2, n')])
	  | _ -> raise (DeductionError "deduction_is: plus")
	end
  | Times ->
	begin
	  match n1, n with
	  | Z, Z -> (TZero, [])
	  | S n1', _ ->
		let n3 = (nat_fun Times) n1' n2 in
		(TSucc, [Is(Times, n1', n2, n3);
				 Is(Plus, n2, n3, n)])
	  | _ -> raise (DeductionError "deduction_is: times")
	end
;;

let rec deduction rel =
  match rel with
  | EvalTo (e, n) ->
	let r, dtree = deduction_eval e n in
	Tr ((rel, r), List.map deduction dtree)
  | Is (op, n1, n2, n) ->
	let r, dtree = deduction_is op n1 n2 n in
	Tr ((rel, r), List.map deduction dtree)
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

let rec pp_dtree_top buf = function
  | Tr ((rel, r), tree) ->
	fprintf buf "%s by %s {@[<v 1>@," (pp_rel rel) (pp_rule r);
	List.iter (pp_dtree buf 1) tree;
	fprintf buf "@]}@.";;
