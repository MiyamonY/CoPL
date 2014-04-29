(***************** Header *******************)
{
  open Printf
  (* 例外 *)
  exception Unexpected_token of string;;

  (* 予約語 *)

  let reservedWords = [
	("evalto", Parser.EVALTO);
  ]
}

(******************* RE *********************)

(****************** Lexer *******************)
rule main = parse
(* 空白文字 *)
|  [' ' '\009' '\012' '\n']+     { main lexbuf }

| "Z" { Parser.ZERO }
| "S" { Parser.SUCC }
| "(" { Parser.LPAREN }
| ")" { Parser.RPAREN }

| "+" { Parser.PLUS }
| "*" { Parser.TIMES }
	
| ['a'-'z'] ['a'-'z']*
    { let id = Lexing.lexeme lexbuf in
      try List.assoc id reservedWords with
      | _ -> raise (Unexpected_token
					  (sprintf "Lexer unexpeded token: %s" id))
    }
		
| eof { exit 0 }

| _ { let token = Lexing.lexeme lexbuf in
	  raise (Unexpected_token
			   (sprintf "Lexer unexpeded token: %s" token))}

(***************** Trailer *******************)
	
