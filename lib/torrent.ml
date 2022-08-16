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

type pieces_controller =
  { received : bool array array
  ; sequested : bool array array
  }

let build_pieces torrent =
  let n_pieces = torrent.piece_length / 20 in
  Printf.printf "Foi? %d " n_pieces;
  let arr =
    Array.init n_pieces (fun i ->
        let blocks_per_piece = calculate_piece_size torrent i in
        Printf.printf "ue, happyface %d " blocks_per_piece;
        Array.make blocks_per_piece false)
  in
  arr
;;

let create_pieces_controller torrent =
  let received = build_pieces torrent in
  let sequested = build_pieces torrent in
  { received; sequested }
;;

type piece_work =
  { index : int
  ; length : int
  ; hash : bytes
  }

let start_download_worker torrent peer _final_buf =
  (* let pieces_work = *)
  (*   Array.mapi *)
  (*     (fun index hash -> *)
  (*       let length = calculate_piece_size torrent index in *)
  (*       { index; length; hash }) *)
  (*     torrent.piece_hashes *)
  (*   |> Array.to_list *)
  (* in *)
  let* client_result =
    Lwt_unix.with_timeout 4. (fun () ->
        Client.connect peer torrent.info_hash torrent.peer_id)
  in
  let client = Result.get_ok client_result in
  let* () =
    Lwt_io.printf
      "Completed handshake with %s\n"
      (Ipaddr.V4.to_string peer.ip)
  in
  let* () = Client.send_unchoke client in
  let* () = Lwt_io.printf "Sending unchoke\n" in
  let* () = Client.send_interested client in
  let* () = Lwt_io.printf "Sending interested\n" in
  (* Lwt_list.iter_s *)
  (*   (fun pw -> *)
  (*     if Bitfield.has_piece client.bitfield pw.index <> true *)
  (*     then Lwt_io.printf "This client does not have piece %d\n" pw.index *)
  (*     else Lwt_io.printf "This client does not have piece %d\n" pw.index) *)
  (*   pieces_work *)
  Lwt.return_unit
;;

let download (torrent : t) =
  let final_buf = Bytes.create torrent.length in
  (* let pieces = create_pieces_controller torrent in *)
  let* () =
    Lwt_list.iter_s
      (fun peer ->
        Lwt.catch
          (fun () -> start_download_worker torrent peer final_buf)
          (fun e ->
            let sexp = Sexplib0.Sexp_conv.sexp_of_exn e in
            let sexp_str = Sexplib0.Sexp.to_string sexp in
            let* () = Logs_lwt.debug (fun m -> m "Error: %s\n" sexp_str) in
            Lwt.return_unit))
      torrent.peers
  in
  Lwt.return final_buf
;;
