open Syntax
open Eval

let print_program = function
  [] -> print_string ""
| exp::_ -> print_string (eval exp)
;;

let rec rep x =
  print_string "# ";
  flush stdout;
  let program = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
  print_string (eval program);
  print_string "\n";
  rep (x + 1)
;;

let _ = rep 0;;
