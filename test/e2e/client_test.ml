open Tornado
open Shared

let find_port () =
  Random.self_init ();
  16_384 + Random.int 10_000
;;

let handler server_handshake flow =
  Tcp.Client.write_bytes flow server_handshake;
  let msg = Tcp.Client.read flow 5 in
  let msg_uppercase = String.uppercase_ascii msg in
  Tcp.Client.write flow msg_uppercase
;;

let successful_handshake_test () =
  let port = find_port () in
  let server_handshake =
    [ 19; 66; 105; 116; 84; 111; 114; 114; 101; 110; 116; 32; 112; 114; 111
    ; 116; 111; 99; 111; 108; 0; 0; 0; 0; 0; 0; 0; 0; 134; 212; 200; 0; 36
    ; 164; 105; 190; 76; 80; 188; 90; 16; 44; 247; 23; 128; 49; 0; 116; 45; 83
    ; 89; 48; 48; 49; 48; 45; 192; 125; 147; 203; 136; 32; 59; 180; 253; 168
    ; 193; 19 ]
    |> Utils.ints_to_bytes
  in
  let info_hash =
    [ 134; 212; 200; 0; 36; 164; 105; 190; 76; 80; 188; 90; 16; 44; 247; 23
    ; 128; 49; 0; 116 ]
    |> Utils.ints_to_bytes
  in
  let client_peer_id =
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20 ]
    |> Utils.ints_to_bytes
  in
  let expected_peer_id =
    [ 45; 83; 89; 48; 48; 49; 48; 45; 192; 125; 147; 203; 136; 32; 59; 180
    ; 253; 168; 193; 19 ]
    |> Utils.ints_to_bytes
  in
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  Eio.Fiber.fork ~sw (fun _ ->
    let flow, _addr = Tcp_server.listen ~net:(Eio.Stdenv.net env) ~sw ~port in
    handler server_handshake flow);
  Eio.Fiber.fork ~sw (fun _ ->
    let host = Ipaddr.V4.localhost in
    let flow = Tcp.Client.open_connection ~env ~sw ~host ~port in
    let handshake_result =
      Client.complete_handshake env flow info_hash client_peer_id
    in
    let h = Result.get_ok handshake_result in
    let exp = Handshake.create info_hash expected_peer_id in
    Check.check_string exp.pstr h.pstr;
    Check.check_bytes exp.info_hash h.info_hash;
    Check.check_bytes exp.peer_id h.peer_id)
;;

let failed_handshake_test () =
  let port = find_port () in
  let server_handshake =
    [ 19; 66; 105; 116; 84; 111; 114; 114; 101; 110; 116; 32; 112; 114; 111
    ; 116; 111; 99; 111; 108; 0; 0; 0; 0; 0; 0; 0; 0; 0xde; 0xe8; 0x6a; 0x7f
    ; 0xa6; 0xf2; 0x86; 0xa9; 0xd7; 0x4c; 0x36; 0x20; 0x14; 0x61; 0x6a; 0x0f
    ; 0xf5; 0xe4; 0x84; 0x3d; 45; 83; 89; 48; 48; 49; 48; 45; 192; 125; 147
    ; 203; 136; 32; 59; 180; 253; 168; 193; 19 ]
    |> Utils.ints_to_bytes
  in
  let info_hash =
    [ 134; 212; 200; 0; 36; 164; 105; 190; 76; 80; 188; 90; 16; 44; 247; 23
    ; 128; 49; 0; 116 ]
    |> Utils.ints_to_bytes
  in
  let client_peer_id =
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20 ]
    |> Utils.ints_to_bytes
  in
  Eio_main.run
  @@ fun env ->
  Eio.Switch.run
  @@ fun sw ->
  Eio.Fiber.fork ~sw (fun _ ->
    let flow, _addr = Tcp_server.listen ~net:(Eio.Stdenv.net env) ~sw ~port in
    handler server_handshake flow);
  Eio.Fiber.fork ~sw (fun _ ->
    let host = Ipaddr.V4.localhost in
    let flow = Tcp.Client.open_connection ~env ~sw ~host ~port in
    let handshake_result =
      Client.complete_handshake env flow info_hash client_peer_id
    in
    let error = Result.get_error handshake_result in
    let is_info_hash_error = error == `Info_hash_is_not_equal in
    Check.check_bool true is_info_hash_error)
;;

let tests =
  ( "client"
  , List.map
      (fun (desc, ty, f) -> Alcotest.test_case desc ty f)
      [ "Successful handshake.", `Quick, successful_handshake_test
      ; "Failed handshake.", `Quick, failed_handshake_test ] )
;;
