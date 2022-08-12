let check_array_char expected actual =
  Alcotest.(check (array char)) "same char array" expected actual
;;

let check_bytes expected actual =
  Alcotest.(check bytes) "same bytes" expected actual
;;

let check_bool expected actual =
  Alcotest.(check bool) "same bool" expected actual
;;

let check_string expected actual =
  Alcotest.(check string) "same string" expected actual
;;
