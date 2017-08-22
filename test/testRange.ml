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

let intersect_tests =
  List.map
    (fun (a, b, result) ->
      (Printf.sprintf "%s /\\ %s = %s"
          (Range.to_string a) (Range.to_string b) (Range.to_string result)) >::
        (fun test_ctxt -> assert_equal (Range.intersect a b) result))
    [
      Range.at_least 0, Range.at_least 0, Range.at_least 0;
      Range.exact 0, Range.exact 0, Range.exact 0;

      Range.at_least 10, Range.at_least 20, Range.at_least 20;
      Range.exact 10, Range.at_least 1, Range.exact 10;
      Range.exact 2, Range.exact 3, Range.Void;
      Range.at_least 20, Range.exact 4, Range.Void
    ]

let range_testslist = [
  "range intersection tests", intersect_tests;
  "range membership tests", membership_tests;
  "range un-membership tests", unmembership_tests;
]

let _ =
  run_throwable_testslist range_testslist
