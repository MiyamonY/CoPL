type rule =
  | EInt
  | EBool
  | EIfT
  | EIfF
  | EPlus
  | EMinus
  | ETimes
  | ELt
  | BPlus
  | BMinus
  | BTimes
  | BLt
  | EPlusBoolL
  | EPlusBoolR
  | EPlusErrorL
  | EPlusErrorR
  | EMinusBoolL
  | EMinusBoolR
  | EMinusErrorL
  | EMinusErrorR
  | ETimesBoolL
  | ETimesBoolR
  | ETimesErrorL
  | ETimesErrorR
  | ELtBoolL
  | ELtBoolR
  | ELtErrorL
  | ELtErrorR
  | EIfInt
  | EIfError
  | EIfTError
  | EIfFError
;;

let pp_rule = function
  | EInt -> "E-Int"
  | EBool -> "E-Bool"
  | EIfT -> "E-IfT"
  | EIfF -> "E-IfF"
  | EPlus -> "E-Plus"
  | EMinus -> "E-Minus"
  | ETimes -> "E-Times"
  | ELt -> "E-Lt"
  | BPlus -> "B-Plus"
  | BMinus -> "B-Minus"
  | BTimes -> "B-Times"
  | BLt -> "B-Lt"
  | EPlusBoolL -> "E-PlusBoolL"
  | EPlusBoolR -> "E-PlusBoolR"
  | EPlusErrorL -> "E-PlusErrorL"
  | EPlusErrorR -> "E-PlusErrorR"
  | EMinusBoolL -> "E-MinusBoolL"
  | EMinusBoolR -> "E-MinusBoolR"
  | EMinusErrorL -> "E-MinusErrorL"
  | EMinusErrorR -> "E-MinusErrorR"
  | ETimesBoolL -> "E-TimesBoolL"
  | ETimesBoolR -> "E-TimesBoolR"
  | ETimesErrorL -> "E-TimesErrorL"
  | ETimesErrorR -> "E-TimesErrorR"
  | ELtBoolL -> "E-LtBoolL"
  | ELtBoolR -> "E-LtBoolR"
  | ELtErrorL -> "E-LtErrorL"
  | ELtErrorR -> "E-LtErrorR"
  | EIfInt -> "E-IfInt"
  | EIfError -> "E-IfError"
  | EIfTError -> "E-IfTError"
  | EIfFError -> "E-IfFError"
;;
