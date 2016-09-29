open OUnit
open Util
open TestUtil
open Syntax

let parse str = Parser.toplevel Lexer.main (Lexing.from_string str)

let parse_expr str =
  let value = parse (str ^ ";") in
  match value with
    [Exp e] -> e
  | _ -> failwith "Not a expression"

let parse_result_test program expr =
  ("test " ^ program) >:: 
    (fun test_ctxt -> assert_equal (parse_expr program) expr)

let parse_tests =
  List.map (fun (x, y) -> parse_result_test x y) [
    "10", Int 10;
    "zero", Zero;
    "succ", Succ;
    "@1/10", Proj(1, 10);
    
    "succ[zero]", Comp(Succ, [Zero]);
    "@1/4[@1/3, @2/3, @3/3, @1/3]", 
      Comp(Proj(1,4), [Proj(1,3); Proj(2,3); Proj(3,3); Proj(1,3)]);
    "succ.zero", Comp(Succ, [Zero]);
    "@1/2 -> zero", PRec(Proj(1,2), Zero);
    "zero()", App(Zero, []);
    "succ(0)", App(Succ, [Int 0]);
    "@1/3(zero(), succ(0), @1/2(2, 3))",
      App (Proj(1, 3),
           [App (Zero, []);
            App (Succ, [Int 0]);
            App (Proj(1,2), [Int 2; Int 3])]);
    "Let x = 1 In x", LetExp("x", Int 1, Var "x");
    "Let f = succ In Let x = succ(42) In f(x)",
      LetExp("f", Succ,
      LetExp("x", app Succ 42,
      App(Var "f", [Var "x"])));
  ]


let prog_eql_test prog1 prog2 =
  prog1 ^ " = " ^ prog2 >::
    (fun test_ctxt -> assert_equal (parse_expr prog1) (parse_expr prog2))


let precedence_tests =
  List.map (fun (s, t) -> prog_eql_test s t) [
    "succ[zero](0)",          "(succ[zero]) (0)";
    "@1/2->zero(0)",          "@1/2 -> (zero(0))";
    "succ.succ(0)",           "succ . (succ(0))";
    "succ.@1/2->zero",        "(succ.@1/2) -> zero";
    "@1/2->@1/2[zero,zero]",  "@1/2 -> (@1/2[zero,zero])";
  ]


let associativity_tests =
  List.map (fun (s, t) -> prog_eql_test s t) [
    "succ.succ.zero",   "succ . (succ.zero)";
    "@1/3->@2/2->zero", "@1/3 -> (@2/2 -> zero)";
  ]


let parser_tests = parse_tests @ precedence_tests @ associativity_tests

let suite = "test parser" >::: parser_tests

let _ = run_throwable_test suite
