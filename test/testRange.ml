open OUnit
open TestUtil

let membership_tests =
  List.map
    (fun (x, r) ->
      (Printf.sprintf "'%d' belongs to %s" x (Range.to_string r)) >::
        (fun test_ctxt -> assert_bool "should be a member" (Range.inner x r)))
    [
      0,  Range.exact 0;
      1,  Range.exact 1;
      10, Range.exact 10;
      0, Range.at_least 0;
      1, Range.at_least 0;
      2, Range.at_least 0;
      10, Range.at_least 5;
      10, Range.at_least 10
    ]

let unmembership_tests =
  List.map
    (fun (x, r) ->
      (Printf.sprintf "'%d' does not belong to %s" x (Range.to_string r)) >::
        (fun test_ctxt -> assert_bool "shouldn't be a member" (not (Range.inner x r))))
    [
      1,  Range.exact 0;
      2,  Range.exact 0;

      9,  Range.exact 10;
      11, Range.exact 10;

      19, Range.at_least 20;
      0, Range.at_least 20;
    ]

let range_tests = membership_tests @ unmembership_tests

let suite = "test range" >::: range_tests

let _ = run_throwable_test suite
