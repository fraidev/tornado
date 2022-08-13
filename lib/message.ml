(* open Stdint *)

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
  | Msg_choke -> 0
  | Msg_unchoke -> 1
  | Msg_interested -> 2
  | Msg_not_interested -> 3
  | Msg_have -> 4
  | Msg_bitfield -> 5
  | Msg_request -> 6
  | Msg_piece -> 7
  | Msg_cancel -> 8
;;

type message =
  { id : message_id
  ; payload : bytes
  }

let format_request index start length =
  let payload = Bytes.create 12 in
  Bytes.set_int32_be payload 0 index;
  Bytes.set_int32_be payload 4 start;
  Bytes.set_int32_be payload 8 length;
  { id = Msg_request; payload }
;;
