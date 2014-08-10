(***************** Header *******************)
{
  open Printf
  (* 例外 *)
  exception Unexpected_token of string;;

  (* 予約語 *)

  let reservedWords = [
	("true", Parser.BOOL true);
	("false", Parser.BOOL false);
	("evalto", Parser.EVALTO);
	("if", Parser.IF);
	("then", Parser.THEN);
	("else", Parser.ELSE);
	("let", Parser.LET);
  ("in", Parser.IN);
  ]
}

(******************* RE *********************)

(****************** Lexer *******************)
rule main = parse
(* 空白文字 *)
|  [' ' '\009' '\012' '\n']+     { main lexbuf }
	
| ";;" { Parser.EOL }

| "-" * ['0' - '9'] ['0' - '9'] *
	{ 
	  Parser.INT (int_of_string (Lexing.lexeme lexbuf))
	}

| "+" { Parser.PLUS }
| "-" { Parser.MINUS }
| "*" { Parser.TIMES }
| "<" { Parser.LT }

| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }
      
| "="	{ Parser.EQ }
| "," { Parser.COMMA }
| "|-"  { Parser.ENV }      
        
| ['a'-'z'] ['a'-'z']*
    { let id = Lexing.lexeme lexbuf in
      try List.assoc id reservedWords with
      | _ -> Parser.VAR id
    }
		
| eof { exit 0 }

| _ { let token = Lexing.lexeme lexbuf in
	  raise (Unexpected_token
			   (sprintf "Lexer unexpeded token: %s" token))}

(***************** Trailer *******************)
	
