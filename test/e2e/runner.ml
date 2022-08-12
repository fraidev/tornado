let () =
  Lwt_main.run
    (Alcotest_lwt.run
       "Lwt Tornado Tests"
       [ Tcp_test.tests; Client_test.tests ])
;;
