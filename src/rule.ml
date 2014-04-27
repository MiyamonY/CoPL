open Printf;;

(* rule *)
type rule =
  | PZero
  | PSucc
  | TZero
  | TSucc;;

let  pp_rule  = function
  | PZero -> "P-Zero"
  | PSucc -> "P-Succ"
  | TZero -> "T-Zero"
  | TSucc -> "T-Succ";;
