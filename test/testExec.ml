open OUnit
open Util
open TestUtil
open Syntax
open Exec

let test_program (filename, expection) = filename >:: fun test_ctxt ->
  let output = ref [] in
  let f id expr ty_e =
    (match id with
      "-" -> output := (expr, ty_e)::(!output)
    | _   -> ())
  in
  let ic = open_in filename in
  let prog = Parser.toplevel Lexer.main (Lexing.from_channel ic) in
  let _ = exec f Environment.empty Environment.empty prog in
  assert_equal (List.rev!output) expection
;;

let test1_expc = [
  Int 0,  TyInt;
  Int 41, TyInt;
  Int 13, TyInt;
  Int 42, TyInt;
  Int 7,  TyInt;
  Int 0,  TyInt;
  Int 42, TyInt;
  Int 42, TyInt;
  ]
;;

let suite =
  "test exec" >::: List.map test_program [
    "sample/1.n0un", test1_expc;
  ]
;;

let _ = run_throwable_test suite
