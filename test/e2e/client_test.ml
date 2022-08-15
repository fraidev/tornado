open Lwt.Syntax
open Tornado
open Shared

let find_port () =
  Random.self_init ();
  16_384 + Random.int 10_000
;;

let skip _ = ()

let handler server_handshake _flow ic oc =
  let hand_len = Bytes.length server_handshake in
  let* () = Lwt_io.write_from_exactly oc server_handshake 0 hand_len in
  let* () = Lwt_io.flush oc in
  let* msg = Lwt_io.read ~count:5 ic in
  let r = String.uppercase_ascii msg in
  Lwt_io.write oc r
;;

let successful_handshake_test _switch () =
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
  let stop, do_stop = Lwt.wait () in
  let* server = Tcp_server.listen ~stop ~port (handler server_handshake) in
  Lwt.async server;
  let* _flow, ic, oc = Tcp.Client.open_connection Ipaddr.V4.localhost port in
  let* handshake_result =
    Client.complete_handshake ic oc info_hash client_peer_id
  in
  let h = Result.get_ok handshake_result in
  let exp = Handshake.create info_hash expected_peer_id in
  Check.check_string exp.pstr h.pstr;
  Check.check_bytes exp.info_hash h.info_hash;
  Check.check_bytes exp.peer_id h.peer_id;
  Lwt.wakeup do_stop ();
  Lwt.return_unit
;;

let failed_handshake_test _switch () =
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
  let stop, do_stop = Lwt.wait () in
  let* server = Tcp_server.listen ~stop ~port (handler server_handshake) in
  Lwt.async server;
  let* _flow, ic, oc = Tcp.Client.open_connection Ipaddr.V4.localhost port in
  let* handshake_result =
    Client.complete_handshake ic oc info_hash client_peer_id
  in
  let error = Result.get_error handshake_result in
  let is_info_hash_error = error == `Info_hash_is_not_equal in
  Check.check_bool true is_info_hash_error;
  Lwt.wakeup do_stop ();
  Lwt.return_unit
;;

let tests =
  ( "client"
  , List.map
      (fun (desc, ty, f) -> Alcotest_lwt.test_case desc ty f)
      [ "Successful handshake.", `Quick, successful_handshake_test
      ; "Failed handshake.", `Quick, failed_handshake_test ] )
;;
