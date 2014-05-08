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
	   | _ -> E
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
	   | _, _ -> E
	 end 
;;

let deduction_if e1 e2 e3 v =
  match eval e1 with
  | I _ as i -> (EIfInt, [EvalTo(e1, i)])
  | B true as b ->
	 let v2 = eval e2 in
	 begin
	 match v2 with
	   | E -> (EIfTError,[EvalTo(e1, B true);
						  EvalTo(e2, v2)])
	   | _ ->
		  if v2 = v then
			(EIfT,[EvalTo(e1, b);
				   EvalTo(e2, v);])
		  else
			raise (DeductionError "deduction_if: result is wrong")
	 end
  | B false as b ->
	 let v3 = eval e3 in
	 begin
	 match v3 with
	   | E -> (EIfFError, [EvalTo(e1, B false);
						   EvalTo(e3, v3)])
	   | _ ->
		  if v3 = v then
			(EIfF, [EvalTo(e1, b);
					EvalTo(e3, v);])
		  else
			raise (DeductionError "deduction_if: result is wrong")
	 end
  | E -> (EIfError,[])
;;

let rec deduction_binop op e1 e2 v =
  let op_rule_normal = function
	| Plus -> EPlus
	| Minus -> EMinus
	| Times -> ETimes
	| Lt -> ELt in
  let op_rule_bool_l = function
	| Plus -> EPlusBoolL
	| Minus -> EMinusBoolL
	| Times -> EMinusBoolL
	| Lt -> ELtBoolL in
  let op_rule_bool_r = function
	| Plus -> EPlusBoolR
	| Minus -> EMinusBoolR
	| Times -> EMinusBoolR
	| Lt -> ELtBoolR in
  let op_rule_error_l = function
	| Plus -> EPlusErrorL
	| Minus -> EMinusErrorL
	| Times -> EMinusErrorL
	| Lt -> ELtErrorL in
  let op_rule_error_r = function
	| Plus -> EPlusErrorR
	| Minus -> EMinusErrorR
	| Times -> EMinusErrorR
	| Lt -> ELtErrorR in
  let v1 = eval e1 in
  let v2 = eval e2 in
  match v1, v2 with
  | I _ , I _  -> (op_rule_normal op,
							 [EvalTo(e1, v1);
							  EvalTo(e2, v2);
							  Is(op, v1, v2, v)])
  | B _, _ -> (op_rule_bool_l op, [EvalTo(e1, v1);])
  | _, B _ -> (op_rule_bool_r op, [EvalTo(e2, v2;)])
  | E, _ -> (op_rule_error_l op, [EvalTo(e1, E);])
  | _, E -> (op_rule_error_r op, [EvalTo(e2, E);])
;;

let rec deduction_eval e v =
  match e with
  | Val (I i) ->
	 (EInt,[])
  | Val (B b) ->
	 (EBool, [])
  | Val E -> raise (EvalError "deduction_eval: error")
  | If (e1, e2, e3) ->
	 deduction_if e1 e2 e3 v
  | BinOp (op, e1, e2) ->
	 deduction_binop op e1 e2 v
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
