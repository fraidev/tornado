open Lwt.Syntax

(* type t = *)
(*   { conn : string *)
(*   ; chocked : bool *)
(*   ; bitfield : string *)
(*   ; peer : Peers.t *)
(*   ; info_hash : string *)
(*   ; peer_id : string *)
(*   } *)

let close_connection fd = Lwt_unix.close fd

let open_connection host port =
  let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let hostname = host in
  let server_addr = (Unix.gethostbyname hostname).Unix.h_addr_list.(0) in
  let sockaddr = Unix.ADDR_INET (server_addr, port) in
  let+ ch_in, ch_out = Lwt_io.open_connection ~fd sockaddr in
  fd, ch_in, ch_out
;;

(* let complete_handshake conn infohash peerID= *)
(*   let task = *)
(*     let handshake_bytes = Bytes.create 20 in *)
(*     (1* let* () = Lwt_io.write out_chan (String.of_bytes handshake_bytes) in *1) *)
(*   in *)
(*   Lwt_unix.with_timeout 3. (fun () -> *)
(*       Lwt_result.ok (task)) *)
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
