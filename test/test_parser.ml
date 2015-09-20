open OUnit
open Syntax

let parse str = Parser.toplevel Lexer.main (Lexing.from_string str)

let parse_result_test program expr =
  ("test " ^ program) >:: 
    (fun test_ctxt -> assert_equal (parse program) expr)

let parser_tests =
  List.map (fun (x, y) -> parse_result_test x y) [
    "10;;", Int 10;
    "zero;;", Zero;
    "succ;;", Succ;
    "@1/10;;", Proj(1, 10);
    "succ[zero];;", Comp(Succ, [Zero]);
    "@1/4[@1/3, @2/3, @3/3, @1/3];;", 
      Comp(Proj(1,4), [Proj(1,3); Proj(2,3); Proj(3,3); Proj(1,3)]);
    "@1/2 -> zero;;", PRec(Proj(1,2), Zero);
    "zero();;", App(Zero, []);
    "succ(0);;", App(Succ, [Int 0]);
    "@1/3(zero(), succ(0), @1/2(2, 3));;",
      App (Proj(1, 3),
           [App (Zero, []);
            App (Succ, [Int 0]);
            App (Proj(1,2), [Int 2; Int 3])]);
]

let suite = "test parser" >::: parser_tests

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

