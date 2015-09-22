open OUnit
open Util
open TestUtil
open Syntax
open Exec
open Eval

let test_program (filename, expection) = filename >:: fun test_ctxt ->
  let output = ref [] in
  let f id expval ty_e =
    (match id with
      "-" -> output := (expval, ty_e)::(!output)
    | _   -> ())
  in
  let ic = open_in filename in
  let prog = Parser.toplevel Lexer.main (Lexing.from_channel ic) in
  let _ = exec f Environment.empty Environment.empty prog in
  assert_equal (List.rev!output) expection
;;

let test1_expc = [
  IntV 0,  TyInt;
  IntV 41, TyInt;
  IntV 13, TyInt;
  IntV 42, TyInt;
  IntV 7,  TyInt;
  IntV 0,  TyInt;
  IntV 42, TyInt;
  IntV 42, TyInt;
  ]
;;

let suite =
  "test exec" >::: List.map test_program [
    "sample/1.n0un", test1_expc;
  ]
;;

let _ = run_throwable_test suite
