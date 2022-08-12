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
      let* line = Tcp.Client.read_line ic in
      let buf = Bytes.unsafe_of_string line in
      let handshake_result = Handshake.read buf in
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
