open Format;;

let pp_string str =
  printf str; print_flush ();;

let rec read_eval_print () =
  let rel = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  Deduction.pp_dtree_top std_formatter (Deduction.deduction rel);
  print_newline ();
  pp_string "# ";
  read_eval_print ();;
  
let _ =
  pp_string "# ";
  read_eval_print ();;

