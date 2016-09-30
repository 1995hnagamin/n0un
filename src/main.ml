open Syntax
open Util

let output_line = function
  Exec.ActBind(id, expval, ty) ->
    print_string id;
    print_string " = ";
    (print_string << Eval.string_of_expval) expval;
    print_string " : ";
    (print_string << string_of_ty) ty;
    print_newline ()
| Exec.ActPrint(expval, ty) ->
    (print_string << Eval.string_of_expval) expval;
    print_newline ()

let print_program env tyenv = Exec.exec output_line env tyenv

let rec repl env tyenv =
  print_string "# ";
  flush stdout;
  let line = read_line () in
  let program = Parser.toplevel Lexer.main (Lexing.from_string line) in
  let (env, tyenv) = print_program env tyenv program in
  repl env tyenv

let exec_file filename =
  let ic = open_in filename in
  let program = Parser.toplevel Lexer.main (Lexing.from_channel ic) in
  let f x = (match x with
      Exec.ActPrint(expval, _) ->
        (print_string << Eval.string_of_expval) expval;
        print_newline ()
    | _ -> ()) in
  Exec.exec f Language.standard_init_env Language.standard_init_tyenv program

let _ =
  if Array.length Sys.argv > 1
  then exec_file Sys.argv.(1)
  else repl Language.standard_init_env Language.standard_init_tyenv
