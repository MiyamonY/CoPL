type rule =
  | TInt
  | TBool
  | TIf
  | TPlus
  | TMinus
  | TMult
  | TLt
  | TVar
  | TLet
  | TAbs
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
  | TMult -> "T-Mult"
  | TLt -> "T-Lt"
  | TVar -> "T-Var"
  | TLet -> "T-Let"
  | TAbs -> "T-Abs"
  | TApp -> "T-App"
  | TLetRec -> "T-LetRec"
  | TNil -> "T-Nil"
  | TCons -> "T-Cons"
  | TMatch -> "T-Match"
;;
