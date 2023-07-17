open Bencode_utils

(* TORRENT *)
type t =
  { announce : string option
  ; info_hash : bytes
  ; piece_hashes : bytes array
  ; piece_length : int64
  ; length : int64
  ; name : string option
  }

let create_with_beencode bencode_root =
  let announce = bencode_to_string bencode_root "announce" in
  let info_beencode = Bencode.dict_get bencode_root "info" |> Option.get in
  let length = bencode_to_int info_beencode "length" |> Option.get in
  let piece_length =
    bencode_to_int info_beencode "piece length" |> Option.get
  in
  let pieces = Bencode.dict_get info_beencode "pieces" |> Option.get in
  let name = bencode_to_string info_beencode "name" in
  let info_hash = sha1_of_bencode info_beencode in
  let piece_hashes = split_piece_hashes pieces in
  { announce; info_hash; piece_hashes; piece_length; length; name }
;;

let open_file input_file =
  let bencode_file = Bencode.decode (`File_path input_file) in
  create_with_beencode bencode_file
;;

let build_tracker_url file peer_id port =
  let announce_url = file.announce |> Option.get in
  let query =
    [ "info_hash", [ Bytes.to_string file.info_hash ]
    ; "peer_id", [ Bytes.to_string peer_id ]; "port", [ Int.to_string port ]
    ; "uploaded", [ "0" ]; "downloaded", [ "0" ]; "compact", [ "1" ]
    ; "left", [ Int64.to_string file.length ] ]
  in
  let uri = Uri.of_string announce_url in
  Uri.add_query_params uri query
;;

