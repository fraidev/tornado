open Lwt.Syntax
open Stdint

type t =
  { in_ch : Lwt_io.input_channel
  ; out_ch : Lwt_io.output_channel
  ; chocked : bool
  ; bitfield : Bitfield.t
  ; peer : Peers.t
  ; info_hash : bytes
  ; peer_id : bytes
  }

let read ic =
  let length_buf = Bytes.create 4 in
  let* _ = Lwt_io.read_into ic length_buf 0 4 in
  let length = Uint32.to_int (Uint32.of_bytes_big_endian length_buf 0) in
  let msg_bytes = Bytes.create length in
  let* _ = Lwt_io.read_into ic msg_bytes 0 length in
  Lwt.return (Message.read length_buf msg_bytes)
;;

let send_request oc index start length =
  let msg = Message.format_request index start length in
  let msg_bytes = Message.serialize (Some msg) in
  Tcp.Client.write_bytes oc msg_bytes
;;

let send_interested t =
  let msg : Message.t =
    { id = Message.id_of_message_type Msg_interested
    ; payload = Bytes.create 0
    }
  in
  let msg_bytes = Message.serialize (Some msg) in
  Tcp.Client.write_bytes t.out_ch msg_bytes
;;

let send_not_interested oc =
  let msg : Message.t =
    { id = Message.id_of_message_type Msg_not_interested
    ; payload = Bytes.create 0
    }
  in
  let msg_bytes = Message.serialize (Some msg) in
  Tcp.Client.write_bytes oc msg_bytes
;;

let send_unchoke t =
  let msg : Message.t =
    { id = Message.id_of_message_type Msg_unchoke; payload = Bytes.create 0 }
  in
  let msg_bytes = Message.serialize (Some msg) in
  Tcp.Client.write_bytes t.out_ch msg_bytes
;;

let send_have oc index =
  let msg = Message.format_have index in
  let msg_bytes = Message.serialize (Some msg) in
  Tcp.Client.write_bytes oc msg_bytes
;;

let complete_handshake ic oc info_hash peerID =
  Lwt_unix.with_timeout 3. (fun () ->
      let handshake = Handshake.create info_hash peerID in
      let* () =
        Logs_lwt.debug (fun m -> m "handshake %s" (Handshake.show handshake))
      in
      let handshake_bytes = Handshake.serialize_to_bytes handshake in
      let* () = Tcp.Client.write_bytes oc handshake_bytes in
      let pstrlen_buf = Bytes.create 1 in
      (* let* result = Lwt_io.read ic in *)
      (* let* () = Logs_lwt.debug (fun m -> m "stringona %s" result) in *)
      let* _ = Lwt_io.read_into ic pstrlen_buf 0 1 in
      let pstrlen = Bytes.get_uint8 pstrlen_buf 0 in
      (* let* () = Logs_lwt.debug (fun m -> m "abc %d" abc) in *)
      let* () = Logs_lwt.debug (fun m -> m "pstrlen %d" pstrlen) in
      let handshake_bytes = Bytes.create (pstrlen + 48) in
      let* _ = Lwt_io.read_into ic handshake_bytes 0 (pstrlen + 48) in
      let* () = Logs_lwt.debug (fun m -> m "handshake_bytes %d" pstrlen) in
      let handshake_result = Handshake.read pstrlen handshake_bytes in
      match handshake_result with
      | Ok h when h.info_hash <> info_hash ->
        Lwt.return_error `Info_hash_is_not_equal
      | Ok h -> Lwt.return_ok h
      | Error e -> Lwt.return_error e)
;;

let recv_bitfield ic =
  Lwt_unix.with_timeout 6. (fun () ->
      let* msg_result = read ic in
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
      | Some m -> Lwt.return_ok m.payload)
;;

let connect (peer : Peers.t) info_hash peer_id =
  Logs.info (fun m -> m "Connecting to %s" (Peers.show peer));
  let* _, in_ch, out_ch = Tcp.Client.open_connection peer.ip peer.port in
  let* () = Lwt_io.printlf "Connected to %s" (Peers.show peer) in
  let* complete_handshake_result =
    complete_handshake in_ch out_ch info_hash peer_id
  in
  match complete_handshake_result with
  | Error e -> Lwt.return_error e
  | Ok _ ->
    let* () = Lwt_io.printl "Completed handshake" in
    let* recv_bitfield_result = recv_bitfield in_ch in
    (match recv_bitfield_result with
    | Error e -> Lwt.return_error e
    | Ok bitfield ->
      Lwt.return_ok
        { in_ch
        ; out_ch
        ; chocked = true
        ; bitfield = Bitfield.of_bytes bitfield
        ; peer
        ; info_hash
        ; peer_id
        })
;;
