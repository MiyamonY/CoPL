/****************** Header *******************/

%{
open Syntax
%}

/****************** Declaration **************/
%token ZERO SUCC

%token LPAREN RPAREN

%token PLUS TIMES
%token REDUCT DETRED MULRED
%token EOL

/****************** Start ********************/
%start toplevel
%type <Syntax.rel> toplevel

/****************** Syntax *******************/
%%

/* 文末区切りがわからないので,EOL(;;)が必要.
   今迄は評価結果が nat だったので必要なかった */
toplevel :
| PlusExp REDUCT PlusExp EOL { SinR ($1, $3) } 
| PlusExp MULRED PlusExp EOL { MulR ($1, $3) }
| PlusExp DETRED PlusExp EOL { DetR ($1, $3) }
	
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
