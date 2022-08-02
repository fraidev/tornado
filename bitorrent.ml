open Bencode_utils

module TorrentFile = struct
  type t =
    { announce : string option
    ; infoHash : bytes
    ; pieceHashes : bytes
    ; pieceLength : int64 option
    ; length : int64 option
    ; name : string option
    }

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
end
