/****************** Header *******************/

%{
open Syntax
%}

/****************** Declaration **************/
%token ZERO SUCC

%token LPAREN RPAREN

%token PLUS TIMES
%token EVALTO

/****************** Start ********************/
%start toplevel
%type <Syntax.rel> toplevel

/****************** Syntax *******************/
%%

toplevel :
| PlusExp EVALTO Nat { EvalTo ($1, $3) }

PlusExp :
| PlusExp PLUS TimesExp { BinOp (Plus, $1, $3) }
| TimesExp { $1 }

TimesExp :
| TimesExp TIMES Term { BinOp (Times, $1, $3) }
| Term { $1 }

Term :
| LPAREN PlusExp RPAREN { $2 }
| Nat { Val $1 }
	
Nat :
| ZERO { Z }
| SUCC LPAREN Nat RPAREN { S ( $3 ) }
