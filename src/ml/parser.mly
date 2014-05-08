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

%nonassoc ELSE
%left LT
%left PLUS MINUS
%left TIMES

%token <int> INT
%token <bool> BOOL
/****************** Start ********************/
%start toplevel
%type <Syntax.rel> toplevel

/****************** Syntax *******************/
%%

toplevel :
| e=Exp EVALTO v=Value EOL { EvalTo (e, v) } 

Exp :
| IF e1=Exp THEN e2=Exp ELSE e3=Exp {If (e1, e2, e3) }
| e1=Exp LT e2=Exp { BinOp (Lt, e1, e2) }
| e1=Exp PLUS e2=Exp { BinOp (Plus, e1, e2) }
| e1=Exp MINUS e2=Exp { BinOp (Minus, e1, e2) }
| e1=Exp TIMES e2=Exp { BinOp (Times, e1, e2) }
| t=Term { t }

Term :
| LPAREN e=Exp RPAREN { e }
| v=Value { Val v }
	
Value :
| i=INT { I i }
| b=BOOL { B b }
