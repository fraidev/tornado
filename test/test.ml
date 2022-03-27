open Ppx_compare_lib.Builtin
open Ppx_sexp_conv_lib
open Conv

let%test "rev" = List.equal Int.equal (List.rev [ 3; 2; 1 ]) [ 1; 2; 3 ]

let%test_unit "List.length" =
  [%test_result: int] (List.length [ 1; 2 ]) ~expect:2
;;

(* let%test_unit "rev2" =
  [%test_result: string] (Bencoding.paser "i3e") ~expect:"a"
;; *)
