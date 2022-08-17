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
[@@deriving show]

type state =
  { requested : int ref
  ; downloaded : int ref
  ; backlog : int ref
  ; buf : bytes
  }

let calculate_block_size piece_length requested =
  (* Last block might be shorter than the typical block *)
  if piece_length - requested < Constants.block_len
  then (
    Log.debug "piece_length: %d" piece_length;
    Log.debug "requested: %d" requested;
    piece_length - requested)
  else Constants.block_len
;;

let read_message (client : Client.t) pw torrent state =
  (* let buf = Bytes.create pw.length in *)
  let* message = Client.read client.in_ch in
  match message with
  | None -> Lwt.return_none
  | Some msg ->
    let* () =
      Logs_lwt.info (fun m -> m "There is msg %s" (Message.to_string message))
    in
    (match Stdint.Uint8.to_int msg.id with
    | 0 ->
      client.chocked := true;
      Lwt.return_none
    | 1 ->
      client.chocked := false;
      Lwt.return_none
    | 4 ->
      (match Message.parse_have msg with
      | Error (`Error e) ->
        let* () =
          Logs_lwt.err (fun m ->
              m "Parse have error for piece: %d, erro: %s" pw.index e)
        in
        Lwt.return_none
      | Ok index ->
        Bitfield.set_piece client.bitfield index;
        Lwt.return_none)
    | 7 ->
      (* let* () = *)
      (*   Logs_lwt.info (fun m -> *)
      (*       m "There is msg %s" (Message.to_string message)) *)
      (* in *)
      (match Message.parse_piece pw.index state.buf msg with
      | Error (`Error e) ->
        let* () =
          Logs_lwt.err (fun m ->
              m "Parse have error for piece: %d, erro: %s" pw.index e)
        in
        Lwt.return_none
      | Ok buf_len ->
        let* () =
          (* final_buf *)
          (* Pieces.add_received pieces_controller pw.index; *)
          state.backlog := !(state.backlog) - 1;
          state.downloaded := !(state.downloaded) + buf_len;
          Logs_lwt.info (fun m ->
              m
                "Downloaded block (%d of %d) of piece (%d of %d)"
                (* !(state.requested) *)
                !(state.downloaded)
                pw.length (*all block size*)
                pw.index
                (Array.length torrent.piece_hashes))
        in
        Lwt.return_none)
    | _ -> Lwt.return_none)
;;

let rec request (client : Client.t) state pw =
  if !(state.backlog) < 1 && !(state.requested) < pw.length
  then
    let* () =
      (* let block_size = calculate_block_size pw.length !(state.requested) in *)
      let block_size = Constants.block_len in
      let* () =
        Logs_lwt.debug (fun m ->
            m
              "pw.index: %d requested: %d block_size: %d "
              pw.index
              !(state.requested)
              block_size)
      in
      let* () =
        Logs_lwt.debug (fun m ->
            m
              "Requesting the block number %d of %d of piece %d"
              ((!(state.requested) / block_size) + 1)
              (* block_size *)
              (pw.length / block_size)
              pw.index)
      in
      let* () =
        Client.send_request client pw.index !(state.requested) block_size
      in
      state.requested := !(state.requested) + block_size;
      state.backlog := !(state.backlog) + 1;
      Lwt.return_unit
    in
    request client state pw
  else Lwt.return_unit
;;

let rec down (client : Client.t) pw torrent state =
  let* _ = read_message client pw torrent state in
  if !(state.downloaded) < pw.length
  then
    let* () =
      if not !(client.chocked)
      then request client state pw
      else Lwt.return_unit
    in
    down client pw torrent state
  else Lwt.return state.buf
;;

let try_download_piece (client : Client.t) pw torrent =
  let state =
    { requested = ref 0
    ; downloaded = ref 0
    ; backlog = ref 0
    ; buf = Bytes.create pw.length
    }
  in
  (* Pieces.add_requested pieces_controller pw.index; *)
  let* () =
    Logs_lwt.info (fun m ->
        m
          "Try to download piece %d of %d"
          pw.index
          (Array.length torrent.piece_hashes))
  in
  Lwt_unix.with_timeout 30. (fun () ->
      let* piece_buf = down client pw torrent state in
      let* () =
        Logs_lwt.info (fun m ->
            m
              "Downloaded piece (%d of %d) with size %d"
              pw.index
              Constants.block_len
              (Bytes.length piece_buf))
      in
      Lwt.return piece_buf)
;;

let start_download_worker torrent peer pieces_controller =
  let pieces_work =
    Array.mapi
      (fun index hash ->
        let length = calculate_piece_size torrent index in
        (* let length = Constants.block_len in *)
        let start = index * Constants.block_len in
        let pw = { index; start; length; hash } in
        pw)
      torrent.piece_hashes
    |> Array.to_list
  in
  let* client_result =
    Lwt_unix.with_timeout 4. (fun () ->
        Client.connect peer torrent.info_hash torrent.peer_id)
  in
  let client = Result.get_ok client_result in
  let* () =
    Logs_lwt.debug (fun m ->
        m "Completed handshake with %s\n" (Ipaddr.V4.to_string peer.ip))
  in
  (* let only_needed_pw = *)
  (*   pieces_work *)
  (*   |> List.filter (fun pw -> Pieces.not_received pieces_controller pw.index) *)
  (* in *)
  (* let* () = *)
  (*   Logs_lwt.debug (fun m -> m "Needed pieces %d" (List.length only_needed_pw)) *)
  (* in *)
  let* () = Client.send_unchoke client in
  let* () = Client.send_interested client in
  Lwt_list.iter_s
    (fun pw ->
      if not (Pieces.needed pieces_controller pw.index)
      then
        Logs_lwt.debug (fun m -> m "This piece (%d) is not needed\n" pw.index)
      else if not (Bitfield.has_piece client.bitfield pw.index)
      then
        Logs_lwt.debug (fun m ->
            m "This client does NOT have piece %d\n" pw.index)
      else
        let* () =
          Logs_lwt.debug (fun m ->
              m "This client does have piece %d\n" pw.index)
        in
        let* _buf = try_download_piece client pw torrent in
        Lwt.return_unit)
    pieces_work
;;

let download (torrent : t) =
  (* let final_buf = Bytes.create torrent.length in *)
  let final_buf = Bytes.create 1 in
  let pieces_controller =
    Pieces.create_pieces_state (Array.length torrent.piece_hashes)
  in
  let* () =
    Lwt_list.iter_s
      (fun peer ->
        Lwt.catch
          (fun () -> start_download_worker torrent peer pieces_controller)
          (fun e ->
            let sexp = Sexplib0.Sexp_conv.sexp_of_exn e in
            let sexp_str = Sexplib0.Sexp.to_string sexp in
            let* () = Logs_lwt.err (fun m -> m "Error: %s\n" sexp_str) in
            Lwt.return_unit))
      torrent.peers
  in
  Lwt.return final_buf
;;
