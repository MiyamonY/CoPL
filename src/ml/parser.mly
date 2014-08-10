/****************** Header *******************/

%{
open Syntax
%}

/****************** Declaration **************/
%token LPAREN RPAREN

%token PLUS TIMES MINUS LT
%token IF THEN ELSE
%token EVALTO
%token EOL
%token ENV
       
%token EQ COMMA
%token LET IN
       
%nonassoc ELSE
%left LT
%left PLUS MINUS
%left TIMES

%token <int> INT
%token <bool> BOOL
%token <string> VAR
              
/****************** Start ********************/
%start toplevel
%type <Syntax.rel> toplevel

/****************** Syntax *******************/
%%

toplevel :
| ENV e=Exp EVALTO v=Value EOL { EvalTo ([], e, v) }
| el=EnvList ENV e=Exp EVALTO v=Value EOL { EvalTo (List.rev el, e, v)}
                                   
EnvList :
| var=VAR EQ v=Value COMMA el=EnvList { (var, v) :: el }
| var=VAR EQ v=Value { [(var, v)] }
  
Exp :
| LET var=VAR EQ e1=Exp IN e2=Exp { Let (var, e1, e2) }
| IF e1=Exp THEN e2=Exp ELSE e3=Exp {If (e1, e2, e3) }
| e1=Exp LT e2=Exp { BinOp (Lt, e1, e2) }
| e1=Exp PLUS e2=Exp { BinOp (Plus, e1, e2) }
| e1=Exp MINUS e2=Exp { BinOp (Minus, e1, e2) }
| e1=Exp TIMES e2=Exp { BinOp (Times, e1, e2) }
| t=Term { t }

Term :
| LPAREN e=Exp RPAREN { e }
| v=Value { Val v }
| v=VAR { Var v }
	
Value :
| i=INT { I i }
| b=BOOL { B b }
