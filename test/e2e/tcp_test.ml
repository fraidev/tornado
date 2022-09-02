open Shared
open Tornado

let port =
  Random.self_init ();
  16_384 + Random.int 10_000
;;

let config = `Port port

let handler flow =
  let msg = Tcp.Client.read flow 5 in
  let msg_uppercase = String.uppercase_ascii msg in
  Tcp.Client.write flow msg_uppercase
;;

let tcp_test () =
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  Eio.Fiber.fork ~sw (fun () ->
    let flow, _addr = Tcp_server.listen ~net:(Eio.Stdenv.net env) ~sw ~port in
    handler flow);
  Eio.Fiber.fork ~sw (fun () ->
    let host = Ipaddr.V4.localhost in
    let flow = Tcp.Client.open_connection ~env ~sw ~host ~port in
    Tcp.Client.write flow "hello";
    let msg = Tcp.Client.read flow 5 in
    Check.check_string "HELLO" msg);
  ()
;;

let tests =
  let open Alcotest in
  ( "TCP"
  , [ test_case
        "TCP client and server should communicate each other."
        `Quick
        tcp_test ] )
;;
