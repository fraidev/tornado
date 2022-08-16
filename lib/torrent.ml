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

let try_download_piece client pw torrent pieces_controller _final_buf =
  Pieces.add_requested pieces_controller pw.start pw.index;
  let* () =
    Logs_lwt.info (fun m ->
        m
          "Try to download piece %d of %d"
          pw.index
          (Array.length torrent.piece_hashes))
  in
  Lwt_unix.with_timeout 30. (fun () ->
      let buf = Bytes.create pw.length in
      let* () = Client.send_request client pw.index pw.start pw.length in
      let* message = Client.read client.in_ch in
      let* () =
        Logs_lwt.info (fun m ->
            m "There is msg %s" (Message.to_string message))
      in
      match message with
      | None -> Lwt.return_unit
      | Some msg ->
        (match Stdint.Uint8.to_int msg.id with
        | 0 ->
          client.chocked := false;
          Lwt.return_unit
        | 1 ->
          client.chocked := true;
          Lwt.return_unit
        | 4 ->
          (match Message.parse_have msg with
          | Error (`Error e) ->
            let* () =
              Logs_lwt.err (fun m ->
                  m "Parse have error for piece: %d, erro: %s" pw.index e)
            in
            Lwt.return_unit
          | Ok index ->
            Bitfield.set_piece client.bitfield index;
            Lwt.return_unit)
        | 7 ->
          (match Message.parse_piece pw.index buf msg with
          | Error (`Error e) ->
            let* () =
              Logs_lwt.err (fun m ->
                  m "Parse have error for piece: %d, erro: %s" pw.index e)
            in
            Lwt.return_unit
          | Ok _len ->
            let* () =
              (* final_buf *)
              Pieces.add_received pieces_controller pw.start pw.index;
              Logs_lwt.info (fun m ->
                  m
                    "Downloaded piece %d of %d"
                    pw.index
                    (Array.length torrent.piece_hashes))
            in
            Lwt.return_unit)
        | _ -> Lwt.return_unit))
;;

let start_download_worker torrent peer pieces_controller final_buf =
  let pieces_work =
    Array.mapi
      (fun index hash ->
        let length = calculate_piece_size torrent index in
        let start = index * Constants.block_len in
        let pw = { index; start; length; hash } in
        (* Logs.info (fun m -> m "Piece work %s" (show_piece_work pw)); *)
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
    Logs_lwt.info (fun m ->
        m "Completed handshake with %s\n" (Ipaddr.V4.to_string peer.ip))
  in
  let* () =
    Logs_lwt.info (fun m ->
        m "Completed handshake with %s\n" (Ipaddr.V4.to_string peer.ip))
  in
  let only_needed_pw =
    pieces_work
    |> List.filter (fun pw ->
           Pieces.needed pieces_controller pw.start pw.index)
  in
  let* () =
    Logs_lwt.info (fun m -> m "Needed pieces %d" (List.length only_needed_pw))
  in
  let* () = Client.send_unchoke client in
  let* () = Client.send_interested client in
  Lwt_list.iter_s
    (fun pw ->
      if not (Pieces.needed pieces_controller pw.start pw.index)
      then
        Logs_lwt.info (fun m -> m "This piece (%d) is not needed\n" pw.index)
      else if not (Bitfield.has_piece client.bitfield pw.index)
      then
        Logs_lwt.info (fun m ->
            m "This client does NOT have piece %d\n" pw.index)
      else
        let* () =
          Logs_lwt.info (fun m ->
              m "This client does have piece %d\n" pw.index)
        in
        let* () = Client.send_request client pw.index pw.start pw.length in
        let* () =
          try_download_piece client pw torrent pieces_controller final_buf
        in
        Logs_lwt.info (fun m ->
            m
              "Downloaded piece %d of %d"
              pw.index
              (Array.length torrent.piece_hashes)))
    only_needed_pw
;;

let download (torrent : t) =
  let final_buf = Bytes.create torrent.length in
  let pieces_controller =
    Pieces.create_pieces_state torrent.length torrent.piece_length
  in
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
