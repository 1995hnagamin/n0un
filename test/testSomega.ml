open OUnit
open Util
open TestUtil

let num x = Somega.Num x
let infty = Somega.Infty

let number_pairs = [
  num 0, num 0;
  num 1, num 0;
  num 1, num 2;
  infty, num 100;
  num 2, infty;
  infty, infty;
]

let concat_map xs f =
  List.concat (List.map f xs)

let consistency_tests =
  concat_map
    number_pairs
    (fun (x, y) ->
      let (sx, sy) = (Somega.to_string x, Somega.to_string y) in
      let (mn, mx) = (Somega.min x y, Somega.max x y) in
      let s = Printf.sprintf "(%s, %s)" sx sy in
      [
        (Printf.sprintf "min%s <= max%s" s s) >::
          (fun test_ctxt ->
            assert_equal true (Somega.le mn mx));
        (Printf.sprintf "%s <> %s -> min%s <> max%s" sx sy s s) >::
          (fun test_ctxt ->
            assert_equal true
            ((Somega.eq x y) || (Somega.less mn mx)));
        (Printf.sprintf "min%s = %s or min%s = %s" s sx s sy) >::
          (fun test_ctxt ->
            assert_equal true
            ((Somega.eq mn x) || (Somega.eq mn y)));
        (Printf.sprintf "max%s = %s or max%s = %s" s sx s sy) >::
          (fun test_ctxt ->
            assert_equal true
            ((Somega.eq mx x) || (Somega.eq mx y)));
      ])

let somega_tests = consistency_tests

let suite = "test Somega" >::: somega_tests

let _ = run_throwable_test suite
