open OUnit
open Util
open TestUtil
open Syntax

let parse str = Parser.toplevel Lexer.main (Lexing.from_string str)

let parse_expr str =
  let value = parse ("Let x = " ^ str ^ ";") in
  match value with
    [LetDecl (_, e)] -> e
  | _ -> failwith "Not a expression"

let parse_result_test program expr =
  ("test " ^ program) >:: 
    (fun test_ctxt -> assert_equal (parse_expr program) expr)

let parse_tests =
  List.map (fun (x, y) -> parse_result_test x y) [
    "10", Int 10;
    "zero", Var "zero";
    "succ", Var "succ";
    "@1/10", proj 1 10;
    "@1", proj_variadic 1;
    "@20", proj_variadic 20;
    
    "succ[zero]", Comp((Var "succ"), [(Var "zero")]);
    "@1/4[@1/3, @2/3, @3/3, @1/3]", 
      Comp(proj 1 4, [proj 1 3; proj 2 3; proj 3 3; proj 1 3]);
    "@2[@3->add, @2/2, @1]",
      Comp(proj_variadic 2,
        [PRec(proj_variadic 3, (Var "add"));
         proj 2 2;
         proj_variadic 1]);
    "succ.zero", Comp((Var "succ"), [(Var "zero")]);
    "@1/2 -> zero", PRec(proj 1 2, (Var "zero"));
    "@1 -> zero", PRec(proj_variadic 1, (Var "zero"));
    "func -> @4", PRec((Var "func"), proj_variadic 4);
    "zero()", App((Var "zero"), []);
    "succ(0)", App((Var "succ"), [Int 0]);
    "@2/3(1, 2, 3)", App(proj 2 3, [Int 1; Int 2; Int 3]);
    "@3(1, 2, 3, 4)", App(proj_variadic 3, [Int 1; Int 2; Int 3; Int 4]);
    "@1/3(zero(), succ(0), @1(2, 3))",
      App (proj 1 3,
           [App ((Var "zero"), []);
            App ((Var "succ"), [Int 0]);
            App (proj_variadic 1, [Int 2; Int 3])]);
    "Let x = 1 In x", LetExp("x", Int 1, Var "x");
    "Let f = succ In Let x = succ(42) In f(x)",
      LetExp("f", (Var "succ"),
      LetExp("x", app (Var "succ") 42,
      App(Var "f", [Var "x"])));
    "Let g = @1 In @1[g,g](1,2,3)",
      LetExp("g", proj_variadic 1,
      App(Comp(proj_variadic 1, [Var "g"; Var "g"]),
          [Int 1; Int 2; Int 3]));
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


let parser_testslist = [
  "string parsing tests", parse_tests;
  "precedence tests", precedence_tests;
  "associativity tests", associativity_tests;
]


let _ =
  run_throwable_testslist parser_testslist
