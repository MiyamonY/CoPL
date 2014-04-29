let rec pp_rule  = function
  | `LSucc -> "L-Succ"
  | `LTrans -> "L-Trans"
  | `LSuccSucc -> "L-SuccSucc"
  | `LZero -> "L-Zero"
  | `LSuccR -> "L-SuccR";;

