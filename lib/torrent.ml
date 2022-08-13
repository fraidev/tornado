type t =
  { peers : Peers.t array
  ; peer_id : bytes
  ; info_hash : bytes
  ; piece_hashes : bytes array
  ; piece_length : int64 option
  ; length : int64 option
  ; name : string option
  }

let create_torrent
    peers
    peer_id
    info_hash
    piece_hashes
    piece_length
    length
    name
  =
  { peers; peer_id; info_hash; piece_hashes; piece_length; length; name }
;;

let download _torrent = assert false
