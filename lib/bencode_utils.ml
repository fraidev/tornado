let bencode_to_string bencode field =
  let bencode_opt = Bencode.dict_get bencode field in
  (* TODO improve this *)
  let bencode_field = Option.get bencode_opt in
  Bencode.as_string bencode_field
;;

let bencode_to_int bencode field =
  let bencode_opt = Bencode.dict_get bencode field in
  (* TODO improve this *)
  let bencode_field = Option.get bencode_opt in
  Bencode.as_int bencode_field
;;

let sha1_of_bencode benconde =
  let bencode_bytes = Bencode_streaming.Encode.to_bytes benconde in
  let bytes = Sha1.digest bencode_bytes in
  bytes
;;

let split_piece_hashes pieces_bencode =
  let hash_len = 20 in
  let buf_s = Bencode.as_string pieces_bencode |> Option.get in
  let buf = Bytes.of_string buf_s in
  let buf_len = Bytes.length buf in
  if buf_len mod hash_len <> 0
  then
    failwith (Printf.sprintf "Received malformed pieces of length %d" buf_len);
  let num_hashes = buf_len / hash_len in
  let hashes = Array.make num_hashes (Bytes.create 20) in
  for i = 0 to num_hashes - 1 do
    let start = i * hash_len in
    let b = Bytes.sub buf start hash_len in
    Array.set hashes i b
  done;
  hashes
;;
