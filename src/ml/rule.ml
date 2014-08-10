type rule =
  | EInt
  | EBool
  | EVar1
  | EVar2
  | EIfT
  | EIfF
  | EPlus
  | EMinus
  | ETimes
  | ELt
  | ELet
  | BPlus
  | BMinus
  | BTimes
  | BLt
;;

let pp_rule = function
  | EInt -> "E-Int"
  | EBool -> "E-Bool"
  | EVar1 -> "E-Var1"
  | EVar2 -> "E-Var2"
  | EIfT -> "E-IfT"
  | EIfF -> "E-IfF"
  | EPlus -> "E-Plus"
  | EMinus -> "E-Minus"
  | ETimes -> "E-Times"
  | ELt -> "E-Lt"
  | ELet -> "E-Let"
  | BPlus -> "B-Plus"
  | BMinus -> "B-Minus"
  | BTimes -> "B-Times"
  | BLt -> "B-Lt"
;;
