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

%token <int> INT
%token <bool> BOOL
/****************** Start ********************/
%start toplevel
%type <Syntax.rel> toplevel

/****************** Syntax *******************/
%%

toplevel :
| LtExp EVALTO Value EOL { EvalTo ($1, $3) } 

LtExp :
| PlusExp LT PlusExp { BinOp (Lt, $1, $3) }
| PlusExp { $1 }

PlusExp :
| PlusExp PLUS TimesExp { BinOp (Plus, $1, $3) }
| PlusExp MINUS TimesExp { BinOp (Minus, $1, $3) }
| TimesExp { $1 }

TimesExp :
| TimesExp TIMES Term { BinOp (Times, $1, $3) }
| Term { $1 }

Term :
| LPAREN PlusExp RPAREN { $2 }
| IF LtExp THEN LtExp ELSE LtExp {If ($2, $4, $6) }
| Value { Val $1 }
	
Value :
| i = INT { I i }
| b = BOOL { B b }
