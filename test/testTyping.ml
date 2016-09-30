open OUnit
open Util
open TestUtil
open Syntax
open Typing

let ty_eql_test = fun (title, expr, ty) ->
  title >:: (fun test_ctxt -> assert_equal (eval_ty Environment.empty expr) ty)
;;

let ty_eql_tests =
  List.map ty_eql_test [
    "0: TyInt", Int 0, TyInt;
    "zero: {}->N(p)", Zero, TyPFun 0;
    "succ: N->N(p)", Succ, TyPFun 1;
    "@1/2: N^2->N(p)", Proj(1,2), TyPFun 2;
    "@3/5: N^5->N(p)", Proj(3,5), TyPFun 5;
    "succ[zero]: {}->N(p)", comp Succ Zero, TyPFun 0;
    "@1/3[@1/2, @2/2, @1/2]: N^2->N(p)",
      Comp(Proj(1,3), [Proj(1,2);Proj(2,2);Proj(1,2)]), TyPFun 2;
    "@1/2->zero: N->N(p)", PRec(Proj(1,2), Zero), TyPFun 1;
    "Let z = 42 In succ(z)",
      LetExp("z", Int 0, App(Succ, [Var "z"])), TyInt;
    "Let f = succ In @2/2[@1/1, f]",
      LetExp("f", Succ, Comp(Proj(2,2), [Proj(1,1); Var "f"])), TyPFun 1;
    "Let add = succ.@3/3->@1/1 In add[@1/3, @3/3] -> @2/2 -> zero",
      LetExp(
        "add",
        PRec(comp Succ (proj 1 3), proj 1 1),
          PRec(Comp(Var "add", [proj 1 3; proj 3 3]),
          PRec(proj 2 2, Zero))),
      TyPFun 2;
  ]
;;

let ty_err_test = fun (title, expr, err) ->
  title >:: 
    (fun test_ctxt -> 
      assert_raises (Typing_error err) (fun () -> eval_ty Environment.empty expr))

let untyped_expr_tests =
  List.map ty_err_test [
    "100(0)", app (Int 100) 0, "Not a function";
    "succ(zero)", App(Succ, [Zero]), "Non-integer object is applied";
    "@1/3[Zero, @1/1, Succ]", Comp(Proj(1,3), [Zero; Proj(1,1); Succ]), "Arities of functions don't match";
    "@1/3[@1/1, Succ]", Comp(Proj(1,3), [Proj(1,1); Succ]), "Arity doesn't match"
  ]
;;

let wrong_apl_tests =
  List.map (fun (x, y) -> ty_err_test (x, y, "Arity doesn't match")) [
    "zero(0)", App(Zero, [Int 0]);
    "succ()", App(Zero, []);
    "succ(1, 2, 3)", apps Zero [1; 2; 3];
    "@1/3(1, 2)", apps (Proj(1,3)) [1; 2];
    "@1/2(1, 2, 3)", apps (Proj(1,2)) [1; 2; 3];
    "(@1/2->zero)()", apps (PRec(Proj(1,2), Zero)) [];
    "(succ.@3/3->@1/1)(1,2,3)",
      apps (PRec(comp Succ (Proj(3,3)), Proj(1,1))) [1; 2; 3];
    "(succ.zero)(1)", app (comp Succ Zero) 1;
    "@1/2[@1/3, @2/3](1)",
      app (Comp (proj 1 2, [proj 1 3; proj 2 3])) 1;
    "Let s = succ In s()", LetExp("s", Succ, App(Var "s", []));
    "Let u = @1/3 In u(1,2)", LetExp("u", proj 1 3, apps (Var "u") [1; 2]);
  ]
;;

let typing_tests =
  ty_eql_tests @ untyped_expr_tests
;;

let suite = "test typing" >::: typing_tests

let _ = run_throwable_test suite
