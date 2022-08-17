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
  { received : bool ref array (* ; requested : bool ref array *)
  ; piece_hashes_len : int
  }

let build_pieces piece_hashes_len =
  Array.init piece_hashes_len (fun _ -> ref false)
;;

let create_pieces_state piece_hashes_len =
  let received = build_pieces piece_hashes_len in
  (* let requested = build_pieces piece_hashes_len in *)
  (* Logs.debug (fun m -> m "requested %d " (Array.length requested)); *)
  { received; piece_hashes_len }
;;

(* let add_requested pc pw_index = pc.requested.(pw_index) := true *)
let add_received pc pw_index = pc.received.(pw_index) := true

(* let needed pc pw_index = *)
(*   (1* Logs.info (fun m -> m "start %d index %d \n" pw_start pw_index); *1) *)
(*   (1* Logs.info (fun m -> m "pc_requested %d \n" (Array.length pc.requested)); *1) *)
(*   (1* let block_index = pw_start / Constants.block_len in *1) *)
(*   (1* Logs.info (fun m -> m "block_index %d \n" block_index); *1) *)
(*   not !(pc.received.(pw_index)) *)
(* ;; *)

let received pc pw_index = !(pc.received.(pw_index))
let is_done pc = Array.for_all (fun a -> !a) pc.received
