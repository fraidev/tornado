open Stdint

type message_id =
  | Msg_choke
  | Msg_unchoke
  | Msg_interested
  | Msg_not_interested
  | Msg_have
  | Msg_bitfield
  | Msg_request
  | Msg_piece
  | Msg_cancel

let id_of_message_type msg =
  match msg with
  | Msg_choke -> Uint8.of_int 0
  | Msg_unchoke -> Uint8.of_int 1
  | Msg_interested -> Uint8.of_int 2
  | Msg_not_interested -> Uint8.of_int 3
  | Msg_have -> Uint8.of_int 4
  | Msg_bitfield -> Uint8.of_int 5
  | Msg_request -> Uint8.of_int 6
  | Msg_piece -> Uint8.of_int 7
  | Msg_cancel -> Uint8.of_int 8
;;

type t =
  { id : uint8
  ; payload : bytes
  }

let to_string msg =
  match msg with
  | Some msg ->
    let msg_str =
      match Uint8.to_int msg.id with
      | 0 -> "Choke"
      | 1 -> "Unchoke"
      | 2 -> "Interested"
      | 3 -> "NotInterested"
      | 4 -> "Have"
      | 5 -> "Bitfield"
      | 6 -> "Request"
      | 7 -> "Piece"
      | 8 -> "Cancel"
      | id -> Printf.sprintf "Unknown#%d" id
    in
    Printf.sprintf "%s [%d]" msg_str (Bytes.length msg.payload)
  | None -> "KeepAlive"
;;

let format_request index start length =
  let payload = Bytes.create 12 in
  Uint32.to_bytes_big_endian index payload 0;
  Uint32.to_bytes_big_endian start payload 4;
  Uint32.to_bytes_big_endian length payload 8;
  { id = id_of_message_type Msg_request; payload }
;;

let format_have index =
  let payload = Bytes.create 4 in
  Uint32.to_bytes_big_endian index payload 0;
  { id = id_of_message_type Msg_have; payload }
;;

(* let format_have index = *)
(*   let payload = Bytes.create 4 in *)
(*   Uint32.to_bytes_big_endian index payload 0; *)
(*   { id = id_of_message_type Msg_have; payload } *)
(* ;; *)
