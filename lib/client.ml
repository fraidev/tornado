open Lwt.Syntax

type t =
  { conn : string
  ; chocked : bool
  ; bitfield : string
  ; peer : Peers.t
  ; info_hash : string
  ; peer_id : string
  }

type erros = [ `Info_hash_is_not_equal ]

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
      | Ok h ->
        if h.info_hash <> info_hash
        then Lwt.return_error `Info_hash_is_not_equal
        else Lwt.return_ok h
      | Error e -> Lwt.return_error e)
;;

let create conn chocked bitfield peer info_hash peer_id =
  { conn; chocked; bitfield; peer; info_hash; peer_id }
;;
