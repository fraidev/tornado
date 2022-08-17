open Bencode_utils
open Lwt.Syntax

(* TORRENT *)
type t =
  { announce : string option
  ; info_hash : bytes
  ; piece_hashes : bytes array
  ; piece_length : int64
  ; length : int64
  ; name : string option
  }
[@@deriving show]

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

let download_file output_file torrent_file =
  let random_peer = Bytes.create 20 in
  let uri = build_tracker_url torrent_file random_peer 6881 in
  let peers = Result.get_ok (Peers.request_peers uri) in
  (* Download *)
  let torrent =
    Torrent.create_torrent
      peers
      random_peer
      torrent_file.info_hash
      torrent_file.piece_hashes
      (torrent_file.piece_length |> Int64.to_int)
      (torrent_file.length |> Int64.to_int)
  in
  let* final_buf = Torrent.download torrent in
  (* Write File *)
  let file_name =
    match output_file, torrent_file.name with
    | Some file, _ -> file
    | _, Some file -> file
    | _, _ -> "torrent_file"
  in
  let* out_ch = Lwt_io.open_file ~mode:Output file_name in
  let* () =
    Lwt_io.write_from_exactly out_ch final_buf 0 (Bytes.length final_buf)
  in
  Lwt.return ()
;;
