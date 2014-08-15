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
  | EMatchM1
  | EMatchM2
  | EMatchN
  | BPlus
  | BMinus
  | BTimes
  | BLt
  | MVar
  | MNil
  | MCons
  | MWild
  | NMConsNil
  | NMNilCons
  | NMConsConsL
  | NMConsConsR
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
  | EMatchM1 -> "E-MatchM1"
  | EMatchM2 -> "E-MatchM2"
  | EMatchN -> "E-MatchN"
  | BPlus -> "B-Plus"
  | BMinus -> "B-Minus"
  | BTimes -> "B-Times"
  | BLt -> "B-Lt"
  | MVar -> "M-Var"
  | MNil -> "M-Nil"
  | MCons -> "M-Cons"
  | MWild -> "M-Wild"
  | NMConsNil -> "NM-ConsNil"
  | NMNilCons -> "NM-NilCons"
  | NMConsConsL -> "NM-ConsConsL"
  | NMConsConsR -> "NM-ConsConsR"
;;
