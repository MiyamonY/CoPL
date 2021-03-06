(****************** Header *******************)

%{
open Syntax
%}

(****************** Declaration **************)
%token LPAREN RPAREN
%token LSBRA RSBRA

%token PLUS TIMES MINUS LT
%token IF THEN ELSE
%token EVALTO
%token EOL
%token ENV
       
%token EQ COMMA
%token LET IN
%token FUN ARROW REC
       
%token CONS MATCH WITH OR
       
%token WILDCARD
       
%left LT
%left PLUS MINUS
%left TIMES
      
         
%token <int> INT
%token <bool> BOOL
%token <Syntax.var> VAR
              
(****************** Start ********************)
%start <Syntax.rel> toplevel

(****************** Syntax *******************)
%%

toplevel :
| ENV e=Exp1 EVALTO v=Value EOL { EvalTo ([], e, v) }
| el=EnvList ENV e=Exp1 EVALTO v=Value EOL { EvalTo (List.rev el, e, v)}
                                   
EnvList :
| var=VAR EQ v=Value COMMA el=EnvList { (var, v) :: el }
| var=VAR EQ v=Value { [(var, v)] }
  
Exp1 :
| LET var=VAR EQ e1=Exp1 IN e2=Exp1 { Let (var, e1, e2) }
| IF e1=Exp1 THEN e2=Exp1 ELSE e3=Exp1 {If (e1, e2, e3) }
| FUN v=VAR ARROW e=Exp1 { Fun (v, e) }
| LET REC var1=VAR EQ FUN var2=VAR ARROW e1=Exp1 IN e2=Exp1
                                   { RecFun (var1, var2, e1, e2) }
| MATCH e=Exp1 WITH c=Clauses  { Match(e, c) }
| e=Exp2 { e }
  
Exp2:                                     
| e1=Exp2 LT e2=Exp2 { BinOp (Lt, e1, e2) }
| e1=Exp2 PLUS e2=Exp2 { BinOp (Plus, e1, e2) }
| e1=Exp2 MINUS e2=Exp2 { BinOp (Minus, e1, e2) }
| e1=Exp2 TIMES e2=Exp2 { BinOp (Times, e1, e2) }
| t=Term1 { t }

Pat :
| p1=Pat1 CONS p2=Pat { PCons(p1, p2) }
| p=Pat1 { p }

Pat1 :
| LPAREN p=Pat RPAREN { p }      
| v=VAR { PVar v }
| LSBRA RSBRA { PNil }
| WILDCARD { PWild }

Clauses :
| p=Pat ARROW e=Exp1 OR c=Clauses { CSec(p, e, c) } 
| p=Pat ARROW e=Exp1 { CPat(p, e) }
  
Term1 :
| h=Term2 CONS tl=Term1 { Cons(h, tl) }
| t=Term2 { t }

Term2:
| t1=Term2  t2=Term3 { App(t1, t2) }
| t=Term3 { t }
          
Term3:
| LPAREN e=Exp1 RPAREN { e }
| i=INT { Val (I i) }
| LSBRA RSBRA { Nil }        
| b=BOOL { Val (B b) }
| v=VAR { Var v }
        
Value :
| v1=Value1 CONS v2=Value { C(v1, v2) }
| v=Value1 { v }

Value1:                         
| i=INT { I i }
| b=BOOL { B b }
| LPAREN RPAREN LSBRA FUN v=VAR ARROW e=Exp1 RSBRA
   { F ([], v, e)}                                             
| LPAREN el=EnvList RPAREN LSBRA FUN v=VAR ARROW e=Exp1 RSBRA
   { F (el, v, e)}
| LSBRA RSBRA { N }
