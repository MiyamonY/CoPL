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

let rec eval_one_step  = function
  | Val n -> Val n
  | BinOp (op, Val n1, Val n2) ->
	Val ((nat_fun op) n1 n2)
  | BinOp (op, e1, e2) ->
	match e1 with
	| Val n -> BinOp(op, e1, eval_one_step e2)
	| _ -> BinOp (op, eval_one_step e1, e2 )	  
;;

let deduction_sinr e e' =
  match e, e' with
  | BinOp (Plus, Val n1, Val n2), Val n3 ->
	(RPlus, [Is(Plus, n1, n2, n3)])
  | BinOp (Times, Val n1, Val n2), Val n3 ->
	(RTimes, [Is(Times, n1, n2, n3)])
  | BinOp (Plus, e1, e2), BinOp(Plus, e1', e2') ->
	if e1 <> e1' then (RPlusL, [SinR(e1, e1')])
	else if e2 <> e2' then (RPlusR, [SinR(e2, e2')])
	else raise
	  (DeductionError "deduction_sinr plus: no reduction occurs")
  | BinOp (Times, e1, e2), BinOp(Times, e1', e2') ->
	if e1 <> e1' then (RTimesL, [SinR(e1, e1')])
	else if e2 <> e2' then (RTimesR, [SinR(e2, e2')])
	else raise
	  (DeductionError "deduction_sinr times: no reduction occurs")
  | _ -> raise (DeductionError "deduction_sinr : there's no deduction rule")  
;;

let deduction_mulr e e' =
  if e = e' then
	(MrZero, [])
  else if (eval_one_step e) = e' then
	(MrOne, [SinR (e, e')])
  else
	let one_step_e = eval_one_step e in
	(MrMulti, [MulR(e, one_step_e);
			   MulR(one_step_e, e')])
;;

let deduction_detr e e' =
  match e, e' with
  | BinOp (Plus, Val n1, Val n2), Val n3 ->
	(DrPlus, [Is (Plus, n1, n2, n3)])
  | BinOp (Times, Val n1, Val n2), Val n3 ->
	(DrTimes, [Is (Times, n1, n2, n3)])
  | BinOp (Plus, Val n1, e2), BinOp (Plus, e1', e2') ->
	if e2 <> e2' then
	  (DrPlusR, [DetR (e2, e2')])
	else raise (DeductionError "deduction_detr plus: no reduction \
  occurs")
  | BinOp (Plus, e1, e2), BinOp (Plus, e1', e2') ->
	if e1 <> e1' && e2 = e2' then
	  (DrPlusL, [DetR (e1, e1')])
	else raise (DeductionError "deduction_detr plus: no reduction \
 occurs")
  | BinOp (Times, Val n1, e2), BinOp (Times, e1', e2') ->
	if e2 <> e2' then
	  (DrTimesR, [DetR (e2, e2')])
	else raise (DeductionError "deduction_detr times: no reduction \
  occurs")
  | BinOp (Times, e1, e2), BinOp (Times, e1', e2') ->
	if e1 <> e1' && e2 = e2' then
	  (DrTimesL, [DetR (e1, e1')])
	else raise (DeductionError "deduction_detr times: no reduction \
 occurs")
  | _ -> raise (DeductionError "deduction_detr : there's no reduction rule ")
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
  let r, dtree =
	match rel with
	| SinR (e, e') -> 
	  deduction_sinr e e'
	| MulR (e, e') ->
	  deduction_mulr e e'
	| DetR (e, e') ->
	  deduction_detr e e'
	| Is (op, n1, n2, n) ->
	  deduction_is op n1 n2 n
  in
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
