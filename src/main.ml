open Syntax
open Util

let print_program = function
  [] -> print_string ""
| exp::_ -> (print_string << string_of_exp << Eval.eval Environment.empty) exp
;;

let rec rep x =
  print_string "# ";
  flush stdout;
  let program = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  (print_string << string_of_ty << Typing.eval_ty Environment.empty) program;
  print_string " : ";
  (print_string << string_of_exp << Eval.eval Environment.empty) program;
  print_string "\n";
  rep (x + 1)
;;

let _ = rep 0;;
