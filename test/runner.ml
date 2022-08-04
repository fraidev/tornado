let () =
  let open Alcotest in
  run "Tornado Tests" [ Bitfield_test.tests ]
;;
