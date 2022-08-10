type t =
  { pstr : string
  ; info_hash : bytes
  ; peer_id : bytes
  }

let create info_hash peer_id =
  { pstr = "BitTorrent protocol"; info_hash; peer_id }
;;

let serialize h =
  let buf = Buffer.create (String.length h.pstr + 49) in
  Buffer.add_uint8 buf (String.length h.pstr);
  Buffer.add_string buf h.pstr;
  Buffer.add_bytes buf (Bytes.make 8 '\000');
  Buffer.add_bytes buf h.info_hash;
  Buffer.add_bytes buf h.peer_id;
  buf
;;

let serialize_to_bytes handshake = serialize handshake |> Buffer.to_bytes

let serialize_to_string handshake =
  handshake |> serialize_to_bytes |> Bytes.to_string
;;

type erros =
  [ `Pstrlen_cannot_be_zero
  | `Not_enough_bytes
  | `Empty
  ]

let read in_ch =
  let open Lwt.Syntax in
  let length_buf = Bytes.create 1 in
  let* () = Lwt_io.read_into_exactly in_ch length_buf 0 1 in
  let pstrlen = Bytes.get_uint8 length_buf 0 in
  if pstrlen = 0
  then Lwt.return_error `Pstrlen_cannot_be_zero
  else (
    let handshake_buf = Bytes.create (48 + pstrlen) in
    let* () = Lwt_io.read_into_exactly in_ch handshake_buf 0 (48 + pstrlen) in
    let info_hash_buffer = Buffer.create 20 in
    let peer_id_buffer = Buffer.create 20 in
    let () =
      Buffer.add_subbytes info_hash_buffer handshake_buf (pstrlen + 8) 20
    in
    let () =
      Buffer.add_subbytes
        peer_id_buffer
        handshake_buf
        (pstrlen + 8 + 20)
        (Bytes.length handshake_buf - (pstrlen + 8 + 20))
    in
    let info_hash = Buffer.to_bytes info_hash_buffer in
    let peer_id = Buffer.to_bytes peer_id_buffer in
    let pstr = Bytes.to_string (Bytes.sub handshake_buf 0 pstrlen) in
    Lwt.return_ok { pstr; info_hash; peer_id })
;;
