let () =
  Alcotest.run "Tornado Tests" [ Bitfield_test.tests; Handshake_test.tests ]
;;
