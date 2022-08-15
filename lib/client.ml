open Lwt.Syntax
open Stdint

type t =
  { conn : string
  ; chocked : bool
  ; bitfield : string
  ; peer : Peers.t
  ; info_hash : string
  ; peer_id : string
  }

let create conn chocked bitfield peer info_hash peer_id =
  { conn; chocked; bitfield; peer; info_hash; peer_id }
;;

let complete_handshake ic oc info_hash peerID =
  Lwt_unix.with_timeout 3. (fun () ->
      let handshake = Handshake.create info_hash peerID in
      let handshake_bytes = Handshake.serialize_to_bytes handshake in
      let* () = Tcp.Client.write_bytes oc handshake_bytes in
      let pstrlen_buf = Bytes.create 1 in
      let* () = Lwt_io.read_into_exactly ic pstrlen_buf 0 1 in
      let pstrlen = Bytes.get_uint8 pstrlen_buf 0 in
      let handshake_bytes = Bytes.create (pstrlen + 48) in
      let* () =
        Lwt_io.read_into_exactly ic handshake_bytes 0 (pstrlen + 48)
      in
      let handshake_result = Handshake.read pstrlen handshake_bytes in
      match handshake_result with
      | Ok h when h.info_hash <> info_hash ->
        Lwt.return_error `Info_hash_is_not_equal
      | Ok h -> Lwt.return_ok h
      | Error e -> Lwt.return_error e)
;;

let recv_bitfield ic =
  Lwt_unix.with_timeout 6. (fun () ->
      let length_buf = Bytes.create 4 in
      let* () = Lwt_io.read_into_exactly ic length_buf 0 4 in
      let length = Uint32.to_int (Uint32.of_bytes_big_endian length_buf 0) in
      let msg_bytes = Bytes.create length in
      let* () = Lwt_io.read_into_exactly ic msg_bytes 0 length in
      let msg_result = Message.read length_buf msg_bytes in
      match msg_result with
      | None ->
        Lwt.return_error
          (`Error
            (Printf.sprintf
               "Expected bitfield but got %s"
               (Message.to_string msg_result)))
      | Some m when m.id <> Message.id_of_message_type Msg_bitfield ->
        Lwt.return_error
          (`Error
            (Printf.sprintf
               "Expected bitfield but got ID %d"
               (Uint8.to_int m.id)))
      | Some m -> Lwt.return_ok m)
;;

(* let connect = *) 
(*   let conn = Tcp.Client.open_connection *)
  
