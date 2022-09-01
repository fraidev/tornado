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

type piece_result =
  { index : int
  ; buf : bytes
  }
[@@deriving show]

type state =
  { requested : int ref
  ; downloaded : int ref
  ; backlog : int ref
  ; buf : bytes
  }

(* Last block might be shorter than the typical block *)
let calculate_block_size piece_length requested =
  if piece_length - requested < Constants.block_len
  then piece_length - requested
  else Constants.block_len
;;

let read_message (client : Client.t) (pw : piece_work) _torrent state =
  let message = Client.read client.flow in
  match message with
  | None -> ()
  | Some msg ->
    let () =
      Logs.debug (fun m -> m "There is msg %s" (Message.to_string message))
    in
    (match Stdint.Uint8.to_int msg.id with
     | 0 -> client.choked := true
     | 1 -> client.choked := false
     | 4 ->
       (match Message.parse_have msg with
        | Error (`Error _e) -> ()
        | Ok index -> Bitfield.set_piece client.bitfield index)
     | 7 ->
       (match Message.parse_piece pw.index state.buf msg with
        | Error (`Error _e) -> ()
        | Ok buf_len ->
          state.backlog := !(state.backlog) - 1;
          state.downloaded := !(state.downloaded) + buf_len)
     | _ -> ())
;;

let rec request (client : Client.t) state pw =
  if !(state.backlog) < 5 && !(state.requested) < pw.length
  then (
    let () =
      let block_size = calculate_block_size pw.length !(state.requested) in
      let () =
        Client.send_request client pw.index !(state.requested) block_size
      in
      state.requested := !(state.requested) + block_size;
      state.backlog := !(state.backlog) + 1;
      ()
    in
    request client state pw)
  else ()
;;

let rec download_piece (client : Client.t) (pw : piece_work) torrent state =
  if !(state.downloaded) < pw.length
  then (
    let () = if not !(client.choked) then request client state pw else () in
    let _ = read_message client pw torrent state in
    download_piece client pw torrent state)
  else state.buf
;;

let try_download_piece env (client : Client.t) pw torrent =
  let state =
    { requested = ref 0
    ; downloaded = ref 0
    ; backlog = ref 0
    ; buf = Bytes.create pw.length
    }
  in
  Logs.info (fun m ->
    m
      "Try to download piece %d of %d"
      (pw.index + 1)
      (Array.length torrent.piece_hashes));
  let clock = Eio.Stdenv.clock env in
  Eio.Time.with_timeout_exn clock 30. (fun () ->
    let piece_buf = download_piece client pw torrent state in
    Logs.info (fun m ->
      m
        "Downloaded piece (%d of %d) with size %d"
        (pw.index + 1)
        (Array.length torrent.piece_hashes)
        (Bytes.length piece_buf));
    piece_buf)
;;

let check_integrity (pw : piece_work) buf =
  let hash = Sha1.digest buf in
  if hash = pw.hash
  then Result.ok ()
  else Result.Error (`Error "Hash mismatch")
;;

let send_pw pieces_work_chan pw =
  Logs.debug (fun m -> m "Try send pw \n");
  Eio.Stream.add pieces_work_chan pw;
  Logs.debug (fun m -> m "sent pw \n")
;;

let rec download_torrent
  env
  (torrent : t)
  (client : Client.t)
  pieces_work_chan
  pieces_result_chan
  =
  let () = Logs.debug (fun m -> m "Try receive pw \n") in
  let pw : piece_work option = Eio.Stream.take_nonblocking pieces_work_chan in
  match pw with
  | None -> ()
  | Some pw ->
    (match
       let () =
         Logs.debug (fun m -> m "Received pw %s\n" (show_piece_work pw))
       in
       if not (Bitfield.has_piece client.bitfield pw.index)
       then (
         send_pw pieces_work_chan pw;
         Logs.debug (fun m ->
           m "This client does NOT have piece %d\n" pw.index))
       else (
         let piece_buf = try_download_piece env client pw torrent in
         let integrity_result = check_integrity pw piece_buf in
         match integrity_result with
         | Error (`Error e) ->
           send_pw pieces_work_chan pw;
           Logs.err (fun m -> m "Error: %s" e)
         | Ok () ->
           Log.debug "Integrity of piece %d is ok." pw.index;
           let piece_result = { buf = piece_buf; index = pw.index } in
           Log.debug "Try send result %d." pw.index;
           Eio.Stream.add pieces_result_chan piece_result;
           Log.debug "Sent result %d." pw.index;
           Client.send_have client pw.index)
     with
     | () ->
       download_torrent env torrent client pieces_work_chan pieces_result_chan
     | exception ex ->
       send_pw pieces_work_chan pw;
       Logs.err (fun m -> m "%a" Fmt.exn ex))
;;

let connect_and_download_torrent
  (env : Eio.Stdenv.t)
  sw
  torrent
  peer
  pieces_work_chan
  pieces_result_chan
  =
  let client_result =
    let clock = Eio.Stdenv.clock env in
    Eio.Time.with_timeout_exn clock 4. (fun () ->
      Client.connect peer torrent.info_hash torrent.peer_id env sw)
  in
  let client = Result.get_ok client_result in
  Logs.debug (fun m ->
    m "Completed handshake with %s\n" (Ipaddr.V4.to_string peer.ip));
  Client.send_unchoke client;
  Client.send_interested client;
  download_torrent env torrent client pieces_work_chan pieces_result_chan
;;

let rec start_work
  env
  sw
  torrent
  (peers : Peers.t list)
  pieces_work_chan
  pieces_result_chan
  =
  match peers with
  | [] -> ()
  | peers_head :: peers_tail ->
    let () =
      Eio.Fiber.fork_sub
        ~sw
        ~on_error:(fun e -> Logs.err (fun m -> m "%a" Fmt.exn e))
        (fun sw ->
          connect_and_download_torrent
            env
            sw
            torrent
            peers_head
            pieces_work_chan
            pieces_result_chan)
    in
    start_work env sw torrent peers_tail pieces_work_chan pieces_result_chan
;;

let download (torrent : t) env sw =
  let final_buf = Bytes.create torrent.length in
  let pieces_hashes_len = Array.length torrent.piece_hashes in
  let pieces_work_chan = Eio.Stream.create 0 in
  let pieces_result_chan = Eio.Stream.create pieces_hashes_len in
  Eio.Fiber.fork ~sw (fun _ ->
    let _ =
      Array.init pieces_hashes_len (fun index ->
        let hash = torrent.piece_hashes.(index) in
        let length = calculate_piece_size torrent index in
        let pw = { index; length; hash } in
        Eio.Stream.add pieces_work_chan pw)
    in
    ());
  Eio.Fiber.fork ~sw (fun _ ->
    start_work
      env
      sw
      torrent
      torrent.peers
      pieces_work_chan
      pieces_result_chan);
  let done_pieces = ref 0 in
  while !done_pieces < pieces_hashes_len do
    let piece_result = Eio.Stream.take pieces_result_chan in
    let length = calculate_piece_size torrent piece_result.index in
    let start, _ = calculate_bounds_for_piece torrent piece_result.index in
    done_pieces := !done_pieces + 1;
    let percent =
      float_of_int !done_pieces /. float_of_int pieces_hashes_len *. 100.
    in
    Logs.app (fun m ->
      m
        "Downloaded (%0.2f%%) - Piece: %d of %d"
        percent
        !done_pieces
        pieces_hashes_len);
    Bytes.blit piece_result.buf 0 final_buf start length
  done;
  final_buf
;;
