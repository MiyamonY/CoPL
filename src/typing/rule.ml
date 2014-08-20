type rule =
  | TInt
  | TBool
  | TIf
  | TPlus
  | TMinus
  | TTimes
  | TLt
  | TVar
  | TLet
  | TFun
  | TApp
  | TLetRec
  | TNil
  | TCons
  | TMatch
;;

let pp_rule = function
  | TInt -> "T-Int"
  | TBool -> "T-Bool"
  | TIf -> "T-If"
  | TPlus -> "T-Plus"
  | TMinus -> "T-Minus"
  | TTimes -> "T-Times"
  | TLt -> "T-Lt"
  | TVar -> "T-Var"
  | TLet -> "T-Let"
  | TFun -> "T-Fun"
  | TApp -> "T-App"
  | TLetRec -> "T-LetRec"
  | TNil -> "T-Nil"
  | TCons -> "T-Cons"
  | TMatch -> "T-Match"
;;
