let piece_len length piece_length piece_index =
  let total_len = length in
  let piece_len = piece_length in
  let last_piece_len = total_len mod piece_len in
  let last_piece_index = total_len / piece_len in
  if last_piece_index = piece_index then last_piece_len else piece_len
;;

let blocks_per_piece length piece_length piece_index =
  let piece_len = piece_len length piece_length piece_index in
  piece_len / Constants.block_len
;;

let calculate_block_len length piece_length piece_index block_index =
  let piece_len = piece_len length piece_length piece_index in
  let last_piece_len = piece_len mod Constants.block_len in
  let last_piece_index = piece_len / Constants.block_len in
  if block_index = last_piece_index
  then last_piece_len
  else Constants.block_len
;;

type pieces_controller =
  { received : bool ref array
  ; piece_hashes_len : int
  }

let build_pieces piece_hashes_len =
  Array.init piece_hashes_len (fun _ -> ref false)
;;

let create_pieces_state piece_hashes_len =
  let received = build_pieces piece_hashes_len in
  { received; piece_hashes_len }
;;

let add_received pc pw_index = pc.received.(pw_index) := true
let received pc pw_index = !(pc.received.(pw_index))
let is_done pc = Array.for_all (fun a -> !a) pc.received
