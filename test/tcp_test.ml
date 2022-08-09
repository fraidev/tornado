open Lwt.Syntax
open Tornado

let taks_server =
  let* s_conn = Tcp.Server.listen ~port:8765 in
  let* () = Tcp.Server.write_line s_conn "hi, I am the server\n" in
  let* r = Tcp.Server.read_line s_conn in
  let* () = Tcp.Server.close_connection s_conn.fd in
  Check.check_string "hey, I am a client\n" r;
  Lwt.return ()
;;

let task_client =
  let* c_conn = Tcp.Client.open_connection "127.0.0.1" 8765 in
  let* () = Tcp.Client.write_line c_conn "hey, I am a client\n" in
  let* r = Tcp.Client.read_line c_conn in
  let* () = Tcp.Client.close_connection c_conn.fd in
  Check.check_string "hi, I am the server" r;
  Lwt.return ()
;;

let tcp_test () =
  let tasks = Lwt.join [ taks_server; task_client ] in
  Lwt_main.run tasks;
;;

let tests =
  let open Alcotest in
  "TCP", [ test_case "TCP client and server should communicate each other." `Quick tcp_test ]
;;
