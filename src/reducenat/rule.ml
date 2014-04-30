type rule =
  | RPlus
  | RTimes
  | RPlusL
  | RPlusR
  | RTimesL
  | RTimesR
  | DrPlus
  | DrTimes
  | DrPlusL
  | DrPlusR
  | DrTimesL
  | DrTimesR
  | MrZero
  | MrMulti
  | MrOne
  | PZero
  | PSucc
  | TZero
  | TSucc
;;

let pp_rule = function
  | RPlus -> "R-Plus"
  | RTimes -> "R-Times"
  | RPlusL -> "R-PlusL"
  | RPlusR -> "R-PlusR"
  | RTimesL -> "R-TimesL"
  | RTimesR -> "R-TimesR"
  | DrPlus -> "DR-Plus"
  | DrTimes -> "DR-Times"
  | DrPlusL -> "DR-PlusL"
  | DrPlusR -> "DR-PlusR"
  | DrTimesL -> "DR-TimesL"
  | DrTimesR -> "DR-TimesR"
  | MrZero -> "MR-Zero"
  | MrMulti -> "MR-Multi"
  | MrOne -> "MR-One"
  | PZero -> "P-Zero"
  | PSucc -> "P-Succ"
  | TZero -> "T-Zero"
  | TSucc -> "T-Succ"
;;
	  
