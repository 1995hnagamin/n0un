open OUnit
open Syntax

let parse str = Parser.toplevel Lexer.main (Lexing.from_string str)

let test_10 test_ctxt     = assert_equal (Int 10) (parse "10;;")
let test_zero test_ctxt   = assert_equal Zero (parse "zero;;")
let test_succ test_ctxt   = assert_equal Succ (parse "succ;;")
let test_proj test_ctxt   = assert_equal ( Proj(1, 10) ) (parse "@1/10;;")
let test_comp1 test_ctxt  = assert_equal ( Comp(Succ, [Zero])  )(parse "succ[zero];;")
let test_comp2 test_ctxt  = assert_equal ( Comp(Proj(1,4), [Proj(1,3); Proj(2,3); Proj(3,3); Proj(1, 3)])) (parse "@1/4[@1/3, @2/3, @3/3, @1/3];;")
let test_prec test_ctxt   = assert_equal ( PRec(Proj(1,2), Zero) ) (parse "@1/2 -> zero;;")

let suite =
  "parser test" >::: [
    "test 10" >:: test_10;
    "test zero" >:: test_zero;
    "test succ" >:: test_succ;
    "test @1/10;;" >:: test_proj;
    "test succ[zero];;" >:: test_comp1;
    "test @1/4[@1/3, @2/3, @3/3, @1/3];;" >:: test_comp2;
    "test @1/2 -> zero;;" >:: test_prec
  ]
;;

exception Failure

let is_failure = function
  RSuccess _ -> false
| _ -> true

let _ = 
  let results = run_test_tt suite in
  if List.exists is_failure results
  then
    raise Failure
  else
    ()

