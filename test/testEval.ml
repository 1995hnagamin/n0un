open OUnit
open Util
open TestUtil
open Syntax
open Eval

let exp_of_expval = function
  IntV n -> Int n
| FunV(f, _) -> f

let eval_eql_test (title, result, expr) =
  title >:: 
    (fun test_ctxt -> assert_equal result ((exp_of_expval << eval Environment.empty) expr))

let prim_eql_tests =
  List.map (fun (x, y) -> eval_eql_test (x, y, y)) [
    "0", Int 0;
    "zero", Zero;
    "succ", Succ;
    "@1/2", proj 1 2;
    "succ.zero", comp Succ Zero;
    "@1/2->zero", PRec(proj 1 2, Zero);
  ]

let eval_eql_tests =
  List.map eval_eql_test [
    "zero()", Int 0, App(Zero, []);
    "succ(0)", Int 1, app Succ 0;
    "@3/3(1,2,succ(3))", Int 4,
      App(proj 3 3, [Int 1; Int 2; App(Succ, [Int 3])]);
    "(@1/2->zero)(43)", Int 42, app (PRec(proj 1 2, Zero)) 43;
    "(succ.@3/3->@1/1)(10, 5)", Int 15,
      apps (PRec(comp Succ (proj 3 3), proj 1 1)) [10; 5];
    "Let x = 10 In x", Int 10, LetExp("x", Int 10, Var "x");
    "Let fo_tw = 42 In succ(fo_tw)", Int 43,
      LetExp("fo_tw", Int 42, App(Succ, [Var "fo_tw"]));
    "Let u = @4/4 In u(1,2,3,4)", Int 4,
      LetExp("u", proj 4 4, apps (Var "u") [1; 2; 3; 4]);
    "Let add = succ.@3/3->@1/1 In Let mul = add[@1/3,@3/3]->@2/2->zero In mul(6, 7)", Int 42,
      LetExp("add", PRec(comp Succ (proj 3 3), proj 1 1),
      LetExp("mul", 
        PRec(Comp(Var "add", [proj 1 3; proj 3 3]),
          PRec(proj 2 2, Zero)),
      apps (Var "mul") [6; 7]));
  ]

let eval_testslist = [
  "tests evaluating a value", prim_eql_tests;
  "tests evaluating a reducible expression ", eval_eql_tests;
]

let _ =
  run_throwable_testslist eval_testslist
