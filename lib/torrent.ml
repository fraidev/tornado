open Lwt.Syntax

type t =
  { peers : Peers.t list
  ; peer_id : bytes
  ; info_hash : bytes
  ; piece_hashes : bytes array
  ; piece_length : int
  ; length : int
  }
[@@deriving show]

let create_torrent peers peer_id info_hash piece_hashes piece_length length =
  { peers; peer_id; info_hash; piece_hashes; piece_length; length }
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

let read_message (client : Client.t) pw _torrent state =
  (* let buf = Bytes.create pw.length in *)
  let* message = Client.read client.in_ch in
  match message with
  | None -> Lwt.return_none
  | Some msg ->
    (* let* () = *)
    (*   Logs_lwt.debug (fun m -> m "There is msg %s" (Message.to_string message)) *)
    (* in *)
    (match Stdint.Uint8.to_int msg.id with
    | 0 ->
      client.choked := true;
      Lwt.return_none
    | 1 ->
      client.choked := false;
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
          Lwt.return_unit
          (* Logs_lwt.info (fun m ->*)
          (*     m*)
          (*       "Downloaded block (%d of %d) of piece (%d of %d)"*)
          (*       (* !(state.requested) *)*)
          (*       !(state.downloaded)*)
          (*       pw.length (*all block size*)*)
          (*       pw.index*)
          (*       (Array.length torrent.piece_hashes))*)
        in
        Lwt.return_none)
    | _ -> Lwt.return_none)
;;

let rec request (client : Client.t) state pw =
  if !(state.backlog) < 5 && !(state.requested) < pw.length
  then
    let* () =
      let block_size = calculate_block_size pw.length !(state.requested) in
      (* let* () = *)
      (*   Logs_lwt.debug (fun m -> *)
      (*       m *)
      (*         "pw.index: %d requested: %d block_size: %d " *)
      (*         pw.index *)
      (*         !(state.requested) *)
      (*         block_size) *)
      (* in *)
      (* let* () = *)
      (*   Logs_lwt.debug (fun m -> *)
      (*       m *)
      (*         "Requesting the block number %d of %d of piece %d" *)
      (*         ((!(state.requested) / block_size) + 1) *)
      (*         (1* block_size *1) *)
      (*         (pw.length / block_size) *)
      (*         pw.index) *)
      (* in *)
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

let rec download_piece (client : Client.t) pw torrent state =
  if !(state.downloaded) < pw.length
  then
    let* () =
      if not !(client.choked)
      then request client state pw
      else Lwt.return_unit
    in
    let* _ = read_message client pw torrent state in
    download_piece client pw torrent state
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
          (pw.index + 1)
          (Array.length torrent.piece_hashes))
  in
  Lwt_unix.with_timeout 30. (fun () ->
      let* piece_buf = download_piece client pw torrent state in
      let* () =
        Logs_lwt.info (fun m ->
            m
              "Downloaded piece (%d of %d) with size %d"
              (pw.index + 1)
              (Array.length torrent.piece_hashes)
              (Bytes.length piece_buf))
      in
      Lwt.return piece_buf)
;;

let check_integrity (pw : piece_work) buf =
  let hash = Sha1.digest buf in
  if hash = pw.hash
  then Result.ok ()
  else Result.Error (`Error "Hash mismatch")
;;

let download_torrent torrent peer pieces_controller final_buf =
  let pieces_work =
    Array.mapi
      (fun index hash ->
        let length = calculate_piece_size torrent index in
        let pw = { index; length; hash } in
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
  let only_needed_pw =
    pieces_work
    |> List.filter (fun pw ->
           not (Pieces.received pieces_controller pw.index))
  in
  let* () =
    Logs_lwt.debug (fun m ->
        m "Needed pieces %d" (List.length only_needed_pw))
  in
  let* () = Client.send_unchoke client in
  let* () = Client.send_interested client in
  Lwt_list.iter_s
    (fun pw ->
      if Pieces.received pieces_controller pw.index
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
        let* piece_buf = try_download_piece client pw torrent in
        let integrity_result = check_integrity pw piece_buf in
        let () =
          match integrity_result with
          | Error (`Error e) -> Log.err "Error: %s" e
          | Ok () ->
            Log.debug "Integrity of piece %d is ok." pw.index;
            Pieces.add_received pieces_controller pw.index;
            let start, _ = calculate_bounds_for_piece torrent pw.index in
            Bytes.blit piece_buf 0 final_buf start pw.length
        in
        let* () = Client.send_have client pw.index in
        Lwt.return_unit)
    only_needed_pw
;;

let rec start_work torrent (peers : Peers.t list) pieces_controller final_buf =
  if Pieces.is_done pieces_controller
  then
    let* () = Logs_lwt.debug (fun m -> m "All pieces are downloaded") in
    Lwt.return_unit
  else (
    match peers with
    | [] -> assert false
    | h :: t ->
      Lwt.catch
        (fun () ->
          let* () = download_torrent torrent h pieces_controller final_buf in
          start_work torrent t pieces_controller final_buf)
        (fun e ->
          let sexp = Sexplib0.Sexp_conv.sexp_of_exn e in
          let sexp_str = Sexplib0.Sexp.to_string sexp in
          let* () = Logs_lwt.err (fun m -> m "Error: %s\n" sexp_str) in
          start_work torrent t pieces_controller final_buf))
;;

let download (torrent : t) =
  let final_buf = Bytes.create torrent.length in
  let pieces_controller =
    Pieces.create_pieces_state (Array.length torrent.piece_hashes)
  in
  let* () = start_work torrent torrent.peers pieces_controller final_buf in
  Lwt.return final_buf
;;
