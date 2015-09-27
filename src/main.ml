open Syntax
open Util

let print_program env tyenv =
  Exec.exec
    (fun id expr ty_e ->
      print_string id;
      print_string " = ";
      (print_string << Eval.string_of_expval) expr;
      print_string " : ";
      (print_string << string_of_ty) ty_e;
      print_newline ())
    env tyenv
;;

let rec repl env tyenv =
  print_string "# ";
  flush stdout;
  let line = read_line () in
  let program = Parser.toplevel Lexer.main (Lexing.from_string line) in
  let (env, tyenv) = print_program env tyenv program in
  repl env tyenv
;;

let exec_file filename =
  let ic = open_in filename in
  let program = Parser.toplevel Lexer.main (Lexing.from_channel ic) in
  print_program Environment.empty Environment.empty program
;;

let print_int =
  let f xs =
    (print_string << string_of_int) (List.nth xs 0);
    print_newline ();
    0 in
  Eval.ActV f
;;

let ienv = Environment.extend "print_int" print_int Environment.empty
let ityenv = Environment.extend "print_int" (TyPFun 1) Environment.empty

let _ =
  if Array.length Sys.argv > 1
  then exec_file Sys.argv.(1)
  else repl ienv ityenv
;;
