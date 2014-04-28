/****************** Header *******************/

%{
open Syntax
%}

/****************** Declaration **************/
%token ZERO SUCC

%token LPAREN RPAREN

%token PLUS TIMES
%token IS

/*** 開始記号 ***/
%start toplevel
%type <Syntax.rel> toplevel

/****************** Syntax *******************/
%%

toplevel :
| PlusExp IS Term { Is ($1, $3) }

PlusExp :
| TimesExp PLUS PlusExp { BinOp(Plus, $1, $3) }
| TimesExp { $1 }

TimesExp :
| Term TIMES TimesExp { BinOp(Times, Val $1, $3) }
| Term { Val $1 }

Term :
| ZERO { Z }
| SUCC ZERO { S Z }
| SUCC LPAREN Term RPAREN { S ($3) }
