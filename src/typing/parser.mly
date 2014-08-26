(****************** Header *******************)

%{
open Syntax
%}

(****************** Declaration **************)
%token LPAREN RPAREN
%token LSBRA RSBRA

%token PLUS TIMES MINUS LT
%token IF THEN ELSE
       
%token EOL
%token ENV
       
%token EQ COMMA
%token LET IN
%token FUN ARROW REC
       
%token CONS MATCH WITH OR
       
%token TYPES
%token DOT
       
%left LT
%left PLUS MINUS
%left TIMES
            
(* Type *)
%token TINT
%token TBOOL
%token TLIST
       
%left ARROW       
%nonassoc TLIST
      
%token <int> INT
%token <bool> BOOL
%token <Syntax.var> VAR
%token <Syntax.tyvar> TVAR
              
(****************** Start ********************)
%start <Syntax.rel> toplevel

(****************** Syntax *******************)
%%

toplevel :
| el=EnvList? ENV e=Exp1 TYPES t=Types EOL
                 { let env_list = (match el with
                                   | Some el' -> List.rev el'
                                   | None -> []) in
                   Types ( env_list, e, t) }
                                   
EnvList :
| var=VAR TYPES t=TyScheme COMMA el=EnvList { (var, t) :: el }
| var=VAR TYPES t=TyScheme { [(var, t)] }
  
Exp1 :
| LET var=VAR EQ e1=Exp1 IN e2=Exp1 { Let (var, e1, e2) }
| IF e1=Exp1 THEN e2=Exp1 ELSE e3=Exp1 {If (e1, e2, e3) }
| FUN v=VAR ARROW e=Exp1 { Fun (v, e) }
| LET REC var1=VAR EQ FUN var2=VAR ARROW e1=Exp1 IN e2=Exp1
                                   { RecFun (var1, var2, e1, e2) }
| MATCH e1=Exp1 WITH LSBRA RSBRA ARROW e2=Exp1 OR v1=VAR CONS v2=VAR ARROW
        e3=Exp2  { Match(e1, e2, v1, v2, e3) }
| e=Exp2 { e }
  
Exp2:                                     
| e1=Exp2 LT e2=Exp2 { BinOp (Lt, e1, e2) }
| e1=Exp2 PLUS e2=Exp2 { BinOp (Plus, e1, e2) }
| e1=Exp2 MINUS e2=Exp2 { BinOp (Minus, e1, e2) }
| e1=Exp2 TIMES e2=Exp2 { BinOp (Times, e1, e2) }
| t=Term1 { t }

Term1 :
| h=Term2 CONS tl=Term1 { Cons(h, tl) }
| t=Term2 { t }
          
Term2:
| t1=Term2 t2=Term3 { App(t1, t2) }
| t=Term3 { t }
          
Term3:
| LPAREN e=Exp1 RPAREN { e }
| i=INT { Val (I i) }
| LSBRA RSBRA { Nil }        
| b=BOOL { Val (B b) }
| v=VAR { Var v }

TyScheme :
| tv=TVAR+ DOT t=Types { (tv, t) }
| t=Types { ([], t) }
          
Types:
| t1=Types1 ARROW t2=Types { Arrow (t1, t2) }
| t=Types1 { t }
           
Types1:
| LPAREN t=Types RPAREN { t }
| TBOOL { Bool }
| TINT { Int }
| tvar=TVAR { TyVar tvar }
| t=Types TLIST { List t }
