let () =
  Alcotest.run "Lwt Tornado Tests" [ Tcp_test.tests; Client_test.tests ]
;;
