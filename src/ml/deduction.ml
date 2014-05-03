open Format;;
open Syntax;;
open Rule;;
open Lib;;

exception DeductionError of string;;
exception EvalError of string;;
  
let rec eval = function
  | Val v -> v
  | If (e1, e2, e3) ->
	 begin
	   match eval e1 with
	   | B true -> eval e2
	   | B false -> eval e3
	   | _ -> raise (EvalError "eval:condiction-clause is not bool")
	 end
  | BinOp (op, e1, e2) ->
	 begin
	   match eval e1, eval e2 with
	   | I i1, I i2 ->
		  begin
			match op with
			| Plus -> I (i1 + i2)
			| Minus -> I (i1 - i2)
			| Times -> I (i1 * i2)
			| Lt -> B (i1 < i2)
		  end
	   | _ -> raise (EvalError "eval: binop operand is not int")
	 end 
;;

let rec deduction_eval e v =
  match e with
  | Val (I i) ->
	 (EInt,[])
  | Val (B b) ->
	 (EBool, [])
  | If (e1, e2, e3) ->
	 begin
	 match eval e1 with
	   | B true as b-> 
		  (EIfT,[EvalTo(e1, b);
				 EvalTo(e2, eval e2);])
	   | B false as b->
		  (EIfF, [EvalTo(e1, b);
				  EvalTo(e3, eval e3);])
	   | _ -> raise (EvalError "deduction_eval: if condition
								clause is not bool")
	 end
  | BinOp (op, e1, e2) ->
	 let r =
	   match op with
	   | Plus -> EPlus
	   | Minus -> EMinus
	   | Times -> ETimes
	   | Lt -> ELt in
	let v1 = eval e1 in
	let v2 = eval e2 in
	 (r, [EvalTo(e1, v1);
		  EvalTo(e2, v2);
		  Is(op, v1, v2, v)])
;;
	  
let deduction_is op n1 n2 n =
  match n1, n2 with
  | I i1, I i2 ->
	 begin
	   match op with
	   | Plus ->
		  if I (i1 + i2) = n then (BPlus, [])
		  else raise (DeductionError "deduction_is: plus")
	   | Minus ->
		  if I (i1 - i2) = n then (BMinus, [])
		  else raise (DeductionError "deduction_is: minus")
	   | Times ->
		  if I (i1 * i2) = n then (BTimes, [])
		  else raise (DeductionError "deduction_is: minus")
	   | Lt -> 
		  if B (i1 < i2) = n then (BLt, [])
		  else raise (DeductionError "deduction_is: lt")
	 end
  |  _ -> raise (EvalError "deduction_is: binop operand is not int")
;;

let rec deduction rel =
  let r, dtree =
	match rel with
	| EvalTo (e, v) -> 
	  deduction_eval e v
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
	end
;;

let rec pp_dtree_top buf = function
  | Tr ((rel, r), tree) ->
	fprintf buf "%s by %s {@[<v 1>@," (pp_rel rel) (pp_rule r);
	List.iter (pp_dtree buf 1) tree;
	fprintf buf "@]}@."
;;
