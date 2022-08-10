open Lwt.Syntax

(* type t = *)
(*   { conn : string *)
(*   ; chocked : bool *)
(*   ; bitfield : string *)
(*   ; peer : Peers.t *)
(*   ; info_hash : string *)
(*   ; peer_id : string *)
(*   } *)

let close_connection fd = Tcp.Client.close_connection fd

let open_connection host port =
  let* c_conn = Tcp.Client.open_connection host port in
  (* let* () = Tcp.Client.write_line c_conn "hey, I am a client\n" in *)
  (* let* r = Tcp.Client.read_line c_conn in *)
  (* let* () = Tcp.Client.close_connection c_conn.fd in *)
  Lwt.return c_conn
;;

(* let complete_handshake conn infohash peerID = *)
(*   let task = *)
(*     let handshake = Handshake.create infohash peerID in *)
(*     let handshake_str = Handshake.serialize_to_string handshake in *)
(* 	(1* Lwt_io.BE. *1) *)
(*     let* () = Tcp.Client.write_line conn handshake_str in *)

(*     let* read_response = Tcp.Client.read_line_char conn in *)
(*     (1* let handshake_response =  read_response in *1) *)
(*     (1* Handshake.read conn *1) *)
(*     (1* let* () = Lwt_io.write out_chan (String.of_bytes handshake_bytes) in *1) *)
(*   in *)
(*   Lwt_unix.with_timeout 3. (fun () -> Lwt_result.ok task) *)
(* ;; *)

(* let receive_bitfield in_chan = *)
(*   let r = input_line in_chan in *)
(*   Printf.printf "Bitfield: %s\n\n" r; *)
(*   flush stdout; *)
(*   r *)

(* let create_client = *)
(*   let* fd, in_ch, out_ch = open_connection "127.0.0.1" 8765 in *)
(*   let* handshake = Lwt_result.get_exn (complete_handshake in_ch out_ch) in *)

(* ;; *)
(* let _s = receive_bitfield in_ch in *)
(* close_connection in_ch *)
