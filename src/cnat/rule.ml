type rule1 =
  | L1Succ 
  | L1Trans;;

type rule2 =
  | L2Zero
  | L2SuccSucc;;

let rec pp_rule  = function
  | `LSucc -> "L-Succ"
  | `LTrans -> "L-Trans"
  | `LSuccSucc -> "L-SuccSucc"
  | `LZero -> "L-Zero";;

