open Stdint

type t =
  { flow : Eio.Flow.two_way
  ; choked : bool ref
  ; bitfield : Bitfield.t
  ; peer : Peers.t
  ; info_hash : bytes
  ; peer_id : bytes
  }

let read flow =
  let length_buf = Tcp.Client.read_bytes flow 4 in
  let length = Uint32.to_int (Uint32.of_bytes_big_endian length_buf 0) in
  let msg_bytes = Tcp.Client.read_bytes flow length in
  Message.read length_buf msg_bytes
;;

let send_request t index start length =
  let msg = Message.format_request index start length in
  let msg_bytes = Message.serialize (Some msg) in
  Tcp.Client.write_bytes t.flow msg_bytes
;;

let send_interested t =
  let msg : Message.t =
    { id = Message.id_of_message_type Msg_interested
    ; payload = Bytes.create 0
    }
  in
  let msg_bytes = Message.serialize (Some msg) in
  Tcp.Client.write_bytes t.flow msg_bytes
;;

let send_not_interested flow =
  let msg : Message.t =
    { id = Message.id_of_message_type Msg_not_interested
    ; payload = Bytes.create 0
    }
  in
  let msg_bytes = Message.serialize (Some msg) in
  Tcp.Client.write_bytes flow msg_bytes
;;

let send_unchoke t =
  let msg : Message.t =
    { id = Message.id_of_message_type Msg_unchoke; payload = Bytes.create 0 }
  in
  let msg_bytes = Message.serialize (Some msg) in
  Tcp.Client.write_bytes t.flow msg_bytes
;;

let send_have client index =
  let msg = Message.format_have (Uint32.of_int index) in
  let msg_bytes = Message.serialize (Some msg) in
  Tcp.Client.write_bytes client.flow msg_bytes
;;

let complete_handshake env flow info_hash peerID =
  let clock = Eio.Stdenv.clock env in
  Eio.Time.with_timeout_exn clock 3. (fun () ->
    let handshake = Handshake.create info_hash peerID in
    let handshake_bytes = Handshake.serialize_to_bytes handshake in
    Tcp.Client.write_bytes flow handshake_bytes;
    let pstrlen_buf = Tcp.Client.read_bytes flow 1 in
    let pstrlen = Bytes.get_uint8 pstrlen_buf 0 in
    let handshake_bytes = Tcp.Client.read_bytes flow (pstrlen + 48) in
    let handshake_result = Handshake.read pstrlen handshake_bytes in
    match handshake_result with
    | Ok h when h.info_hash <> info_hash ->
      Result.error `Info_hash_is_not_equal
    | Ok h -> Result.ok h
    | Error e -> Result.error e)
;;

let recv_bitfield flow env =
  let clock = Eio.Stdenv.clock env in
  Eio.Time.with_timeout clock 6. (fun () ->
    let msg_result = read flow in
    match msg_result with
    | None ->
      Result.error
        (`Error
          (Printf.sprintf
             "Expected bitfield but got %s"
             (Message.to_string msg_result)))
    | Some m when m.id <> Message.id_of_message_type Msg_bitfield ->
      Result.error
        (`Error
          (Printf.sprintf
             "Expected bitfield but got ID %d"
             (Uint8.to_int m.id)))
    | Some m -> Result.ok m.payload)
;;

let connect (peer : Peers.t) info_hash peer_id env sw =
  (* Log.debug "Connecting to %s" (Peers.show peer); *)
  let flow =
    Tcp.Client.open_connection ~env ~sw ~host:peer.ip ~port:peer.port
  in
  (* Logs.debug (fun m -> m "Connected to %s" (Peers.show peer)); *)
  let complete_handshake_result =
    complete_handshake env flow info_hash peer_id
  in
  match complete_handshake_result with
  | Error e -> Result.error e
  | Ok _ ->
    let () = Logs.debug (fun m -> m "Completed handshake") in
    let recv_bitfield_result = recv_bitfield flow env in
    (match recv_bitfield_result with
     | Error e -> Result.error e
     | Ok bitfield ->
       Result.ok
         { flow
         ; choked = ref true
         ; bitfield = Bitfield.of_bytes bitfield
         ; peer
         ; info_hash
         ; peer_id
         })
;;
