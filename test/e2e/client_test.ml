open Lwt.Syntax
open Tornado
open Shared

let handler _socket (ic, oc) =
  let server_handshake =
    [ 19; 66; 105; 116; 84; 111; 114; 114; 101; 110; 116; 32; 112; 114; 111
    ; 116; 111; 99; 111; 108; 0; 0; 0; 0; 0; 0; 0; 0; 134; 212; 200; 0; 36
    ; 164; 105; 190; 76; 80; 188; 90; 16; 44; 247; 23; 128; 49; 0; 116; 45; 83
    ; 89; 48; 48; 49; 48; 45; 192; 125; 147; 203; 136; 32; 59; 180; 253; 168
    ; 193; 19 ]
    |> Utils.ints_to_bytes
  in
  let rec loop () =
    let* s = Lwt_io.read_line ic in
    let* () = Lwt_io.printf "Server received: %s" s in
    let server_handshake_str = Bytes.unsafe_to_string server_handshake in
    let* () = Lwt_io.write_line oc server_handshake_str in
    loop ()
  in
  loop ()
;;

let successful_handshake_test switch () =
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
  let* server = Tcp_server.listen handler ~port:8767 in
  let* c_conn = Tcp.Client.open_connection "localhost" 8767 in
  Lwt_switch.add_hook (Some switch) (fun () ->
      Lwt.join
        [ Tcp_server.close_connection server
        ; Tcp.Client.close_connection c_conn.fd ]);
  let* handshake_result =
    Client.complete_handshake c_conn info_hash client_peer_id
  in
  let h = Result.get_ok handshake_result in
  let exp = Handshake.create info_hash expected_peer_id in
  Check.check_string exp.pstr h.pstr;
  Check.check_bytes exp.info_hash h.info_hash;
  Check.check_bytes exp.peer_id h.peer_id;
  Lwt.return_unit
;;

let tests =
  let open Alcotest_lwt in
  "Client", [ test_case "handshake_test." `Quick successful_handshake_test ]
;;
