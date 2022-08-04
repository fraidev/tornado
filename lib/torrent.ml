open Bencode_utils

module File = struct
  type t =
    { announce : string option
    ; infoHash : bytes
    ; pieceHashes : bytes array
    ; pieceLength : int64 option
    ; length : int64 option
    ; name : string option
    }
  [@@deriving show]

  let create_with_beencode bencode_root =
    let announce = bencode_to_string bencode_root "announce" in
    let info_beencode = Bencode.dict_get bencode_root "info" |> Option.get in
    let length = bencode_to_int info_beencode "length" in
    let pieceLength = bencode_to_int info_beencode "piece length" in
    let pieces = Bencode.dict_get info_beencode "pieces" |> Option.get in
    let name = bencode_to_string info_beencode "name" in
    let infoHash = sha1_of_bencode info_beencode in
    let pieceHashes = split_piece_hashes pieces in
    { announce; infoHash; pieceHashes; pieceLength; length; name }
  ;;

  let build_tracker_url file peer_id port =
    let announce_url = file.announce |> Option.get in
    let query =
      [ "info_hash", [ Bytes.to_string file.infoHash ]
      ; "peer_id", [ Bytes.to_string peer_id ]
      ; "port", [ Int.to_string port ]
      ; "uploaded", [ "0" ]
      ; "downloaded", [ "0" ]
      ; "compact", [ "1" ]
      ; "left", [ Int64.to_string (file.length |> Option.get) ]
      ]
    in
    let uri = Uri.of_string announce_url in
    Uri.add_query_params uri query
  ;;
end
