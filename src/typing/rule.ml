type rule =
  | EInt
  | EBool
  | EVar
  | EIfT
  | EIfF
  | EPlus
  | EMinus
  | ETimes
  | ELt
  | ELet
  | EFun
  | EApp
  | ELetRec
  | EAppRec
  | ENil
  | ECons
  | EMatchNil
  | EMatchCons
  | BPlus
  | BMinus
  | BTimes
  | BLt
;;

let pp_rule = function
  | EInt -> "E-Int"
  | EBool -> "E-Bool"
  | EVar -> "E-Var"
  | EIfT -> "E-IfT"
  | EIfF -> "E-IfF"
  | EPlus -> "E-Plus"
  | EMinus -> "E-Minus"
  | ETimes -> "E-Times"
  | ELt -> "E-Lt"
  | ELet -> "E-Let"
  | EFun -> "E-Fun"
  | EApp -> "E-App"
  | ELetRec -> "E-LetRec"
  | EAppRec -> "E-AppRec"
  | ENil -> "E-Nil"
  | ECons -> "E-Cons"
  | EMatchNil -> "E-MatchNil"
  | EMatchCons -> "E-MatchCons"
  | BPlus -> "B-Plus"
  | BMinus -> "B-Minus"
  | BTimes -> "B-Times"
  | BLt -> "B-Lt"
;;
