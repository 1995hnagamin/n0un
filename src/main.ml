open Syntax
open Util

let print_program = function
  [] -> print_string ""
| (Exp expr)::_ ->
    (print_string << string_of_ty << Typing.eval_ty Environment.empty) expr;
    print_string " : ";
    (print_string << string_of_exp << Eval.eval Environment.empty) expr;
    print_newline ();
;;

let rec rep x =
  print_string "# ";
  flush stdout;
  let line = read_line () in
  let program = Parser.toplevel Lexer.main (Lexing.from_string line) in
  print_program program;
  rep (x + 1)
;;

let exec_file filename =
  let ic = open_in filename in
  let program = Parser.toplevel Lexer.main (Lexing.from_channel ic) in
  print_program program
;;

let _ =
  if Array.length Sys.argv > 1
  then exec_file Sys.argv.(1)
  else rep 0
;;
