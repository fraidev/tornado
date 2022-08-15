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

let parse_piece index buf msg =
  match msg with
  | m when m.id <> id_of_message_type Msg_piece ->
    Result.error
      (`Error
        (Printf.sprintf
           "Expected PIECE (ID %d), got ID %d"
           (Uint8.to_int (id_of_message_type Msg_piece))
           (Uint8.to_int msg.id)))
  | m when Bytes.length m.payload < 8 ->
    Result.error
      (`Error
        (Printf.sprintf "Payload too short. %d < 8" (Bytes.length m.payload)))
  | m when Uint32.to_int (Uint32.of_bytes_big_endian m.payload 0) <> index ->
    Result.error
      (`Error
        (Printf.sprintf
           "Expected index %d, got %d"
           index
           (Uint32.to_int (Uint32.of_bytes_big_endian m.payload 0))))
  | m ->
    let start = Uint32.to_int (Uint32.of_bytes_big_endian m.payload 4) in
    (match start with
    | s when s > Bytes.length buf ->
      Result.error
        (`Error
          (Printf.sprintf
             "Start offset too high. %d >= %d"
             start
             (Bytes.length buf)))
    | s ->
      let data = Bytes.sub msg.payload 8 (Bytes.length msg.payload - 8) in
      (match data with
      | d when s + Bytes.length d > Bytes.length buf ->
        Result.error
          (`Error
            (Printf.sprintf
               "Data too long [%d] for offset %d with length %d"
               (Bytes.length data)
               start
               (Bytes.length buf)))
      | d ->
        Bytes.blit data 0 buf s (Bytes.length data);
        Result.ok (Bytes.length d)))
;;

let parse_have msg =
  match msg with
  | m when m.id <> id_of_message_type Msg_have ->
    Result.error
      (`Error
        (Printf.sprintf
           "Expected HAVE (ID %d), got ID %d"
           (Uint8.to_int (id_of_message_type Msg_have))
           (Uint8.to_int msg.id)))
  | m when Bytes.length m.payload <> 4 ->
    Result.error
      (`Error
        (Printf.sprintf
           "Expected payload length 4, got length %d"
           (Bytes.length m.payload)))
  | m ->
    let index = Uint32.to_int (Uint32.of_bytes_big_endian m.payload 0) in
    Result.ok index
;;

let serialize msg =
  match msg with
  | None -> Bytes.create 4
  | Some m ->
    let length = Bytes.length m.payload + 1 in
    let buf = Bytes.create (4 + length) in
    Uint32.to_bytes_big_endian (Uint32.of_int length) buf 0;
    Bytes.set buf 4 (Char.chr (Uint8.to_int m.id));
    Bytes.blit m.payload 0 buf (4 + 1) (Bytes.length m.payload);
    buf
;;

let read length_buf message_buf =
  let length = Uint32.to_int (Uint32.of_bytes_big_endian length_buf 0) in
  match length with
  | 0 -> None
  | _ ->
    let id = Uint8.of_int (Char.code (Bytes.get message_buf 0)) in
    let payload = Bytes.sub message_buf 1 (length - 1) in
    Some { id; payload }
;;
