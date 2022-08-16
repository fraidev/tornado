open Lwt.Syntax

type t =
  { peers : Peers.t list
  ; peer_id : bytes
  ; info_hash : bytes
  ; piece_hashes : bytes array
  ; piece_length : int
  ; length : int
  ; name : string
  }
[@@deriving show]

let create_torrent
    peers
    peer_id
    info_hash
    piece_hashes
    piece_length
    length
    name
  =
  { peers; peer_id; info_hash; piece_hashes; piece_length; length; name }
;;

let calculate_bounds_for_piece torrent index =
  let start = index * torrent.piece_length in
  let final = start + torrent.piece_length in
  let torrent_length = torrent.length in
  if final > torrent_length then start, torrent_length else start, final
;;

let calculate_piece_size torrent index =
  let start, final = calculate_bounds_for_piece torrent index in
  final - start
;;

type piece_work =
  { index : int
  ; start : int
  ; length : int
  ; hash : bytes
  }

let block_len = 16384

let start_download_worker torrent peer _pieces_controller _final_buf =
  let pieces_work =
    Array.mapi
      (fun index hash ->
        let length = calculate_piece_size torrent index in
        let start = index * block_len in
        { index; start; length; hash })
      torrent.piece_hashes
    |> Array.to_list
  in
  let* client_result =
    Lwt_unix.with_timeout 4. (fun () ->
        Client.connect peer torrent.info_hash torrent.peer_id)
  in
  let client = Result.get_ok client_result in
  let* () =
    Logs_lwt.info (fun m ->
        m "Completed handshake with %s\n" (Ipaddr.V4.to_string peer.ip))
  in
  let* () = Client.send_unchoke client in
  let* () = Client.send_interested client in
  Lwt_list.iter_s
    (fun pw ->
      if not (Bitfield.has_piece client.bitfield pw.index)
      then
        Logs_lwt.info (fun m ->
            m "This client does NOT have piece %d\n" pw.index)
      else
        let* () =
          Logs_lwt.info (fun m ->
              m "This client does have piece %d\n" pw.index)
        in
        let* () = Client.send_request client pw.index pw.start pw.length in
        let* message = Client.read client.in_ch in
        Logs_lwt.info (fun m -> m "there msg %s" (Message.to_string message)))
        (* match message with *)
        (* | None -> Logs_lwt.info (fun m -> m "KEEP ALIVE") *)
        (* | Some msg -> *)
    pieces_work
;;

let piece_len (torrent : t) piece_index =
  let total_len = torrent.length in
  let piece_len = torrent.piece_length in
  let last_piece_len = total_len mod piece_len in
  let last_piece_index = total_len / piece_len in
  if last_piece_index = piece_index then last_piece_len else piece_len
;;

let blocks_per_piece (torrent : t) piece_index =
  let piece_len = piece_len torrent piece_index in
  piece_len / block_len
;;

let calculate_block_len torrent piece_index block_index =
  let piece_len = piece_len torrent piece_index in
  let last_piece_len = piece_len mod block_len in
  let last_piece_index = piece_len / block_len in
  if block_index = last_piece_index then last_piece_len else block_len
;;

type pieces_controller =
  { received : bool ref array array
  ; requested : bool ref array array
  }

let build_pieces (torrent : t) =
  let n_pieces = torrent.piece_length / 20 in
  let arr =
    Array.init n_pieces (fun i ->
        let blocks_per_piece = blocks_per_piece torrent i in
        Array.make blocks_per_piece (ref false))
  in
  arr
;;

let create_pieces_controller torrent =
  let received = build_pieces torrent in
  let requested = build_pieces torrent in
  { received; requested }
;;

let add_requested pc (piece_block : piece_work) =
  let block_index = piece_block.start / block_len in
  pc.requested.(piece_block.index).(block_index) := true
;;

let add_received pc (piece_block : piece_work) =
  let block_index = piece_block.start / block_len in
  pc.received.(piece_block.index).(block_index) := true
;;

let needed pc (piece_block : piece_work) =
  let block_index = piece_block.start / block_len in
  not !(pc.requested.(piece_block.index).(block_index))
;;

let is_done pc =
  Array.for_all (fun arr -> Array.for_all (fun a -> !a) arr) pc.received
;;

let download (torrent : t) =
  let final_buf = Bytes.create torrent.length in
  let pieces_controller = create_pieces_controller torrent in
  let* () =
    Lwt_list.iter_s
      (fun peer ->
        Lwt.catch
          (fun () ->
            start_download_worker torrent peer pieces_controller final_buf)
          (fun e ->
            let sexp = Sexplib0.Sexp_conv.sexp_of_exn e in
            let sexp_str = Sexplib0.Sexp.to_string sexp in
            let* () = Logs_lwt.debug (fun m -> m "Error: %s\n" sexp_str) in
            Lwt.return_unit))
      torrent.peers
  in
  Lwt.return final_buf
;;
