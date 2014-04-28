type rule =
  | LSucc 
  | LTrans;;

let rec pp_rule  = function
  | LSucc -> "L-Succ"
  | LTrans -> "L-Trans";;

