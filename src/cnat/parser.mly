/****************** Header *******************/

%{
  open Syntax
%}

/****************** Declaration **************/
%token ZERO SUCC

%token LPAREN RPAREN

%token IS LESS THAN

/******************* 開始記号 *****************/
%start toplevel
%type <Syntax.rel> toplevel

/****************** Syntax *******************/
%%

toplevel :
| Term IS LESS THAN Term { Less ($1, $5) }

Term :
| ZERO { Z }
| SUCC ZERO { S Z }
| SUCC LPAREN Term RPAREN { S ($3) }
