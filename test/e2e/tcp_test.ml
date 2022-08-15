open Lwt.Syntax
open Shared
open Tornado

let port =
  Random.self_init ();
  16_384 + Random.int 10_000
;;

let config = `Port port

let handler _flow ic oc =
  let* msg = Lwt_io.read ~count:5 ic in
  let r = String.uppercase_ascii msg in
  Lwt_io.write oc r
;;

let tcp_test _switch () =
  let stop, do_stop = Lwt.wait () in
  let* server = Tcp_server.listen ~stop ~port handler in
  let client_test () =
    let* _flow, ic, oc =
      Tcp.Client.open_connection Ipaddr.V4.localhost port
    in
    let* () = Lwt_io.write oc "hello" in
    let* msg = Lwt_io.read ic ~count:5 in
    Check.check_string "HELLO" msg;
    Lwt_io.close ic
  in
  Lwt.async server;
  let* () = client_test () in
  Lwt.wakeup do_stop ();
  Lwt.return_unit
;;

let tests =
  let open Alcotest_lwt in
  ( "TCP"
  , [ test_case
        "TCP client and server should communicate each other."
        `Quick
        tcp_test ] )
;;
