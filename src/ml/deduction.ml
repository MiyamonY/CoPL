open Format;;
open Syntax;;
open Rule;;
open Lib;;

exception DeductionError of string;;
exception EvalError of string;;
exception MatchError of string;;
  
let rec match_pat p v =
  match p with
  | PVar v' -> Some [(v', v)]
  | PNil ->
     begin
     match v with
     | N -> Some []
     | _ -> None
     end
  | PCons (p1, p2) ->
     begin
     match v with
     | C (v1, v2) ->
        begin
        match match_pat p1 v1 ,match_pat p2 v2 with
        | Some env1, Some env2 -> Some (env2 @ env1)
        | _, _ -> None
        end
     | _ -> None
     end
  | PWild ->
     Some []
;;

let rec match_clauses p v =
  match p with
  | CPat (p, e) ->
     begin
     match match_pat p v with
     | Some env' -> Some (env', e)
     | None -> None
     end
  | CSec (p, e, c) ->
     begin
     match match_pat p v with
     | Some env' -> Some (env', e)
     | None -> match_clauses c v
     end
;;     

let rec eval env = function
  | Var v -> List.assoc v env
  | Val v -> v
  | If (e1, e2, e3) ->
	 begin
	   match eval env e1 with
	   | B true -> eval env e2
	   | B false -> eval env e3
	   | _ -> raise (EvalError "eval:condiction-clause is not bool")
	 end
  | BinOp (op, e1, e2) ->
	 begin
	   match eval env e1, eval env e2 with
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
  | Let (var, e1, e2) ->
     let v1 = eval env e1 in
     eval ((var, v1)::env) e2
  | Fun (v, e) ->
     F(env, v, e)
  | App (e1, e2) ->
     let v1 = eval env e1 in
     let v2 = eval env e2 in
     begin
       match v1 with
       | F (env', v, e0) ->
          eval ((v, v2)::env') e0
       | RF (env2, x, y, e0) ->
          eval ((y,v2)::(x, v1)::env2) e0
       | _ -> raise (EvalError "eval: not appled to function")
     end
  | RecFun (var1, var2, e1, e2) ->
     let new_env = (var1, RF(env, var1, var2, e1))::env in
     eval new_env e2
  | Nil -> N
  | Cons(e1, e2) ->
     C (eval env e1, eval env e2)
  | Match (e, c) ->
     let v = eval env e in
     match match_clauses c v with
     | None -> raise (EvalError "eval: not match")
     | Some (env', e') ->
        eval (env'@ env) e'
;;

let rec deduction_eval env e v =
  match e with
  | Var var ->
     (EVar, [])
  | Val (I i) ->
	   (EInt,[])
  | Val (B b) ->
	   (EBool, [])
  | Val N ->
     (ENil, [])
  | Val (C(v1, v2)) ->
     raise (DeductionError "deduction_eval: \
                            no deduction rule for cons value" )
  | Val F(_, _ , _) ->
     raise (DeductionError "deduction_eval: \
                            no deduction rule for function value")
  | Val RF(_, _, _, _) ->
     raise (DeductionError "deduction_eval: \
                            no deduction rule for recursive function value")
  | If (e1, e2, e3) ->
	 begin
	 match eval env e1 with
	   | B true as b-> 
		    (EIfT,[EvalTo(env, e1, b);
				       EvalTo(env, e2, eval env e2);])
	   | B false as b->
		    (EIfF, [EvalTo(env, e1, b);
				        EvalTo(env, e3, eval env e3);])
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
	let v1 = eval env e1 in
	let v2 = eval env e2 in
	 (r, [EvalTo(env, e1, v1);
		    EvalTo(env, e2, v2);
		    Is(op, v1, v2, v)])
  | Let (var, e1, e2) ->
     let v1 = eval env e1 in
     let new_env = (var, v1)::env in
     let v2 = eval new_env e2 in
     (ELet, [EvalTo(env, e1, v1);
             EvalTo(new_env, e2, v2)])
  | Fun (var, e) ->
     (EFun, [])
  | App (e1, e2) ->
     let v1 = eval env e1 in
     let v2 = eval env e2 in
     begin
     match v1 with
     | F (env', var, e0) ->
        let new_env = (var, v2) :: env' in
        let v = eval new_env e0 in
        (EApp, [EvalTo(env, e1, v1);
                EvalTo(env, e2, v2);
                EvalTo(new_env, e0, v)])
     | RF (env2, x, y, e0) ->
        let new_env = (y, v2)::(x, v1)::env2 in
        (EAppRec, [EvalTo(env, e1, v1);
                   EvalTo(env, e2, v2);
                   EvalTo(new_env, e0, v)])
     | _ -> raise (DeductionError "deduction_eval: left exp is not function")
     end
  | RecFun(x, y, e1, e2) ->
     let new_env = (x, RF(env, x, y, e1))::env in
     (ELetRec, [EvalTo(new_env, e2, v);])
  | Nil -> (ENil,[])
  | Cons(e1, e2) ->
     let v1 = eval env e1 in
     let v2 = eval env e2 in
     (ECons, [EvalTo(env, e1, v1);
              EvalTo(env, e2, v2)])
  | Match(e0, CPat(p, e)) ->
     let v0 = eval env e0 in
     let env1 =
       match match_pat p v0 with
       | Some env' -> env'
       | None -> raise (MatchError "deduction_eval: pattern doesn't match")
     in
     let env2 = env1 @ env in
     (EMatchM1, [EvalTo(env, e0, v0);
                 Matches(p, v0, env1);
                 EvalTo(env2, e, v)])
  | Match(e0, CSec(p, e, c)) ->
     let v0 = eval env e0 in
     match match_pat p v0 with
     | None ->
        (EMatchN, [EvalTo(env, e0, v0);
                    NotMatches(p, v0);
                    EvalTo(env, Match(e0, c), v)])
     | Some e1 ->
        let e2 = e1 @ env in
        (EMatchM2, [EvalTo(env, e0, v0);
                    Matches(p, v0, e1);
                    EvalTo(e2, e, v)])
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

let rec deduction_matches p v env =
  let rec has_var_pat p =
    match p with
    | PVar x -> [x]
    | PCons (p1, p2) ->
       (has_var_pat p1) @ (has_var_pat p2)
    | _ -> []
  in
  match p with
  | PVar _ -> (MVar, [])
  | PNil -> (MNil, [])
  | PCons(p1, p2) ->
     begin
     match v with
     | C(v1, v2) ->
        let var_in_p1 = has_var_pat p1 in
        let env1 = List.map (fun x -> (x, List.assoc x env)) var_in_p1 in
        let env2 = List.fold_right (fun x i -> List.remove_assoc x i)
                                   var_in_p1 env in
       (MCons, [Matches (p1, v1, env1);
                Matches (p2, v2, env2);])
     | _ ->
        raise (MatchError "deduction_matches: pattern doesn't match")
     end
  | PWild -> (MWild, [])
;;
  
let rec deduction_not_matches p v =
  match p, v with
  | PNil, C(_, _) -> (NMConsNil, [])
  | PCons (_, _), N -> (NMNilCons, [])
  | PCons (p1, p2), C(v1, v2) ->
     begin
       match match_pat p1 v1 with
       | Some _ -> (NMConsConsR, [NotMatches (p2, v2)])
       | None -> (NMConsConsL, [NotMatches (p1, v1)])
     end
  | _, _ -> raise (MatchError "deduction_not_matches: pattern does match")
;;

let rec deduction rel =
  let r, dtree =
	match rel with
	| EvalTo (env, e, v) ->
	   deduction_eval env e v
	| Is (op, n1, n2, n) ->
	   deduction_is op n1 n2 n
  | Matches (p, v, env) ->
     deduction_matches p v env
  | NotMatches (p, v) ->
     deduction_not_matches p v
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
