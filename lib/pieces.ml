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
  { received : bool ref array array
  ; requested : bool ref array array
  }

let build_pieces length piece_length =
  let n_pieces = piece_length / 20 in
  let arr =
    Array.init n_pieces (fun piece_index ->
        let blocks_per_piece =
          blocks_per_piece length piece_length piece_index
        in
        Array.make blocks_per_piece (ref false))
  in
  arr
;;

let create_pieces_state length piece_length =
  let received = build_pieces length piece_length in
  let requested = build_pieces length piece_length in
  Logs.debug (fun m -> m "requested %d " (Array.length requested));
  { received; requested }
;;

let add_requested pc pw_start pw_index =
  let block_index = pw_start / Constants.block_len in
  pc.requested.(pw_index).(block_index) := true
;;

let add_received pc pw_start pw_index =
  let block_index = pw_start / Constants.block_len in
  pc.received.(pw_index).(block_index) := true
;;

let needed pc pw_start pw_index =
  (* Logs.info (fun m -> m "start %d index %d \n" pw_start pw_index); *)
  (* Logs.info (fun m -> m "pc_requested %d \n" (Array.length pc.requested)); *)
  let block_index = pw_start / Constants.block_len in
  (* Logs.info (fun m -> m "block_index %d \n" block_index); *)
  not !(pc.requested.(pw_index).(block_index))
;;

let is_done pc =
  Array.for_all (fun arr -> Array.for_all (fun a -> !a) arr) pc.received
;;
