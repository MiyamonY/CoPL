type rule =
  | EConst
  | EPlus
  | ETimes
  | PZero
  | PSucc
  | TZero
  | TSucc;;

let  pp_rule  = function
  | EConst -> "E-Const"
  | EPlus -> "E-Plus"
  | ETimes -> "E-Times"
  | PZero -> "P-Zero"
  | PSucc -> "P-Succ"
  | TZero -> "T-Zero"
  | TSucc -> "T-Succ";;
