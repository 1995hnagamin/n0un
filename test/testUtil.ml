open OUnit
exception Failure

let is_failure = function
  RSuccess _ -> false
| _ -> true
;;

let run_throwable_test suite =
  let results = run_test_tt suite in
  if List.exists is_failure results
  then raise Failure
  else results
;;

let run_throwable_suites suites =
  let f suite =
    let _ = run_throwable_test suite in ()
  in
  List.iter f suites
