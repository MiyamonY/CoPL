open Format;;

let num = ref 1;;

Arg.parse
  [("-n", Arg.Set_int num, "The number of CompareNat: 1 - 3\
 (default is 1)")]
  (fun _ -> raise (Arg.Bad "invalid arg"))
  "";;

let pp_string str =
  printf str; print_flush ();;

let rec read_eval_print () =
  let rel = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  (Deduction.pp_dtree_top std_formatter (Deduction.deduction !num rel));
  print_newline ();
  pp_string "# ";
  read_eval_print ();;
  
let _ =
  if 1 <= !num && !num <= 3 then
	begin
	  pp_string "# "; read_eval_print ()
	end
  else failwith ("invalid arg: " ^ (string_of_int !num));;

