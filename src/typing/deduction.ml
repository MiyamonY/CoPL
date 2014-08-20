open Format;;
open Syntax;;
open Rule;;
open Lib;;

exception TypingDeductionError of string;;
exception TypeError of string;;
exception TypeInferenceError of string;;
  
let fresh_ty_var =
  let counter = ref 0 in
  let body () =
    let ret = !counter in
    counter := !counter + 1;
    TyVar ret
  in
  body;;
  
type subst = (tyvar * types) list;;
  
let rec pp_subst subst =
  let pp_subst_aux (ty1, ty2) =
    sprintf "(%s : %s)" (pp_types ty1) (pp_types ty2)
  in
  String.concat "," (List.map pp_subst_aux subst)
;;
  
let rec subst_ty subst = function
  | Int -> Int
  | Bool -> Bool
  | List t -> List (subst_ty subst t)
  | TyVar n ->
     begin
       try subst_ty subst (List.assoc (TyVar n) subst) with
       | Not_found -> TyVar n
     end
  | Arrow (t1, t2) -> Arrow (subst_ty subst t1, subst_ty subst t2)
;;

let subst_ty_env subst env =
  let subst_env (var, ty) = (var, subst_ty subst ty) in
  List.map subst_env env
;;
  
let rec unify = function
  | [] -> []
  | (ty1, ty2) :: tl ->
     let unify_aux subst_one substed =
       List.map (fun (x,y) -> (subst_ty [subst_one] x, subst_ty [subst_one] y)) substed in
     begin
       match ty1, ty2 with
       | x, y when x = y -> unify tl
       | List t1, List t2 when t1 <> t2 -> unify ((t1, t2)::tl)
       | TyVar n as ty_var, (_ as ex_ty) ->
          (ty_var, ex_ty) :: unify (unify_aux (ty_var, ex_ty) tl)
       | _ as ex_ty, (TyVar n as ty_var) ->
          (ty_var, ex_ty) :: unify (unify_aux (ty_var, ex_ty) tl)
       | List t1, List t2 -> unify ((t1, t2)::tl)
       | Arrow (t1, t2), Arrow (t1', t2') -> unify ((t1, t1')::(t2, t2') ::tl)
       | _ -> raise (TypeInferenceError "unification error")
     end;;

let rec infer_types env = function
  | Var var ->
     ([], List.assoc var env)
  | Val (I _) -> ([], Int)
  | Val (B _) -> ([], Bool)
  | If (e1, e2, e3) ->
     let s1, t1 = infer_types env e1 in
     let s2, t2 = infer_types env e2 in
     let s3, t3 = infer_types env e3 in
     ((t1, Bool)::(t2, t3)::s1@s2@s3, t2)
  | BinOp (op, e1, e2) ->
     let s1, t1 = infer_types env e1 in
     let s2, t2 = infer_types env e2 in
     let ty =
       begin
         match op with
         | Plus | Minus | Times -> Int
	       | Lt -> Bool
       end in
     ((t1, Int)::(t2, Int)::s1@s2, ty)
  | Let (var, e1, e2) ->
     let s1, t1 = infer_types env e1 in
     let s2, t2 = infer_types ((var,t1)::env) e2 in
     (s1@s2, t2)
  | Fun (var, e) ->
     let alpha = fresh_ty_var () in
     let s1, t1 = infer_types ((var, alpha)::env) e in
     (s1, Arrow(alpha, t1))
  | App (e1, e2) ->
     let alpha = fresh_ty_var () in
     let s1, ty1 = infer_types env e1 in
     let s2, ty2 = infer_types env e2 in
     ((ty1, Arrow(ty2, alpha))::s1@s2, alpha)
  | RecFun (x, y, e1, e2) ->
     let alpha = fresh_ty_var () in
     let beta = fresh_ty_var () in
     let s1, t1 = infer_types ((x, Arrow (alpha, beta))::(y, alpha)::env) e1 in
     let s2, t = infer_types ((y,alpha)::env) e2 in
     (s1@s2, t)
  | Nil ->
     let alpha = fresh_ty_var () in
     ([], List alpha)
  | Cons (e1, e2) ->
     let s1, t1 = infer_types env e1 in
     let s2, t2 = infer_types env e2 in
     begin
       match t2 with
       | List t ->
          ((t1, t)::s1@s2, t2)
       | TyVar n ->
          let alpha = fresh_ty_var () in
          ((t1, alpha)::(t2, List alpha)::s1@s2, t2)
       | _ -> raise (TypeInferenceError "cons inference error")
     end
  | Match (e1, e2, x, y, e3) ->
     let s1, t1 = infer_types env e1 in
     let s2, t2 = infer_types env e2 in
     begin
       match t1 with
       | List t1' ->
          let new_env = (x, t1')::(y, List t1')::env in
          let s3, t3 = infer_types new_env e3 in
          ((t2, t3)::s1@s2@s3, t3)
       | (TyVar _) as ty_var ->
          let alpha = fresh_ty_var () in
          let new_env = (x, alpha)::(y, List alpha)::env in
          let s3, t3 = infer_types new_env e3 in
          ((ty_var, List alpha)::(t2, t3)::s1@s2@s3, t3)
       | _ -> raise (TypeInferenceError ("match can't infer" ^ (pp_types t1)))
     end
;;

let infer_type env e =
  let subst, t = infer_types env e in
  let t_infer = subst_ty (unify subst) t in
  (subst, t_infer)
;;
  
let rec deduction_types env e ty =
  match e with
  | Var var ->
     (TVar, [])
  | Val (I _) ->
	   (TInt,[])
  | Val (B _) ->
	   (TBool, [])
  | If (e1, e2, e3) ->
     (TIf, [Types(env, e1, Bool);
            Types(env, e2, ty);
            Types(env, e3, ty);])
  | BinOp (op, e1, e2) ->
	   let r = match op with
	     | Plus -> TPlus
 	     | Minus -> TMinus
	     | Times -> TTimes
	     | Lt -> TLt
     in
     (r, [Types (env, e1, Int);
          Types (env, e2, Int);])
  | Let (var, e1, e2) ->
     let subst1, ty1 = infer_type env e1 in
     let new_env = (var, ty1) :: env in
     let subst2, ty2 = infer_type new_env e2 in
     let new_subst = unify @@ (ty, ty2)::subst1 @ subst2 in
     let ty1' = subst_ty new_subst ty1 in
     let new_env' = subst_ty_env new_subst new_env in
     (TLet, [Types (env, e1, ty1');
             Types (new_env', e2, ty)])
  | Fun (var, e1) ->
     begin
       match ty with
       | Arrow (ty1, ty2) ->
          let new_env = (var, ty1) :: env in
          (TFun, [Types (new_env, e1, ty2);])
       | _ -> raise (TypingDeductionError "deduction_types: type isn't Arrow")
     end
  | App (e1, e2) ->
     let subst2, ty2 = infer_type env e2 in
     let subst1, ty1 = infer_type env e1 in
     begin
     match ty1 with
     | Arrow(ty1s, ty1d) ->
        let ty2' = subst_ty (unify @@ (ty1d, ty)::(ty1s, ty2)::subst2) ty2 in
        (TApp, [Types (env, e1, Arrow (ty2', ty));
                Types (env, e2, ty2');])
     | _ -> raise (TypingDeductionError "deduction_types: type isn't Arrow")
     end
  | RecFun(x, y, e1, e2) ->
     let alpha = fresh_ty_var () in
     let beta = fresh_ty_var () in
     let new_env1 = (y, alpha)::(x, Arrow(alpha, beta))::env in
     let new_env2 = (x, Arrow(alpha, beta))::env in
     let subst1, ty1 = infer_type new_env1 e1 in
     let subst2, ty2 = infer_type new_env2 e2 in
     let new_subst = unify @@ (ty2, ty) :: (beta, ty1) :: subst1 @ subst2 in
     let ty1' = subst_ty new_subst ty1 in
     let ty2' = subst_ty new_subst ty2 in
     let new_env1' = subst_ty_env new_subst new_env1 in
     let new_env2' = subst_ty_env new_subst new_env2 in
     (TLetRec, [Types(new_env1', e1, ty1');
                Types(new_env2', e2, ty2')])
  | Nil -> (TNil, [])
  | Cons(e1, e2) ->
     begin
     match ty with
     | List ty' ->
        (TCons, [Types (env, e1, ty');
                 Types (env, e2, ty)])
     | _ -> raise (TypingDeductionError "deduction_types: type insn't List")
     end
  | Match(e1, e2, x, y, e3) ->
     let subst1, ty1 = infer_type env e1 in
     let subst2, ty2 = infer_type env e2 in
     let new_env = 
       begin
         match ty1 with
         | List ty1' -> (y, ty1)::(x, ty1')::env
         | _ -> raise (TypingDeductionError "deduction_types: type insn't List")
       end in
     let subst3, ty3 = infer_type new_env e3 in
     let new_subst = unify @@ (ty2, ty)::(ty3, ty)::subst1 @ subst2 @ subst3 in
     let ty1' = subst_ty new_subst ty1 in
     let ty2' = subst_ty new_subst ty2 in
     let ty3' = subst_ty new_subst ty3 in
     let new_env' = subst_ty_env new_subst new_env in
     (TMatch, [Types (env, e1, ty1');
               Types (env, e2, ty2');
               Types (new_env', e3, ty3')])
;;

let rec deduction rel =
  let r, dtree =
	match rel with
	| Types (env, e, ty) ->
	   deduction_types env e ty
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
