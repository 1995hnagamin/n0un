open OUnit
open Syntax

let parse str = Parser.toplevel Lexer.main (Lexing.from_string str)

let parse_result_test program expr =
  ("test " ^ program) >:: 
    (fun test_ctxt -> assert_equal (parse program) expr)

let parse_tests =
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
;;

let prog_eql_test prog1 prog2 =
  prog1 ^ " = " ^ prog2 >::
    (fun test_ctxt -> assert_equal (parse prog1) (parse prog2))
;;

let precedence_tests =
  List.map (fun (s, t) -> prog_eql_test s t) [
    "succ[zero](0);;",          "(succ[zero]) (0);;";
    "@1/2->zero(0);;",          "@1/2 -> (zero(0));;";
    "@1/2->@1/2[zero,zero];;",  "@1/2 -> (@1/2[zero,zero]);;";
  ]
;;

let associativity_tests =
  List.map (fun (s, t) -> prog_eql_test s t) [
    "@1/3->@2/2->zero;;", "@1/3 -> (@2/2 -> zero);;";
  ]
;;

let parser_tests = parse_tests @ precedence_tests @ associativity_tests

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

