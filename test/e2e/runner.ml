let () =
  Alcotest.run "Tornado Tests E2E" [ Tcp_test.tests; Client_test.tests ]
;;
