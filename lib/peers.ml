open Stdint
open Ipaddr
open Lwt.Syntax

type t =
  { ip : V4.t
  ; port : int
  }
[@@deriving show]

let create peers_bin =
  let peer_size = 6 in
  let peers_bin_length = Bytes.length peers_bin in
  let num_peers = peers_bin_length / peer_size in
  if peers_bin_length mod peer_size <> 0
  then failwith "Received malformed peers";
  List.init num_peers (fun i ->
      let ip_size = 4 in
      let offset = i * peer_size in
      let ip_bin = Bytes.sub peers_bin offset ip_size in
      let port =
        Uint16.of_bytes_big_endian peers_bin (offset + ip_size)
        |> Uint16.to_int
      in
      let ip_array =
        Array.init ip_size (fun j ->
            Uint8.of_bytes_big_endian ip_bin j |> Uint8.to_int)
      in
      let ip =
        Ipaddr.V4.make ip_array.(0) ip_array.(1) ip_array.(2) ip_array.(3)
      in
      { ip; port })
;;

let request_peers uri =
  let* _resp, body = Cohttp_lwt_unix.Client.get (uri) in
  let* body_string = Cohttp_lwt.Body.to_string body in
  let tracker_bencode = Bencode.decode (`String body_string) in
  let peers_string =
    Bencode_utils.bencode_to_string tracker_bencode "peers" |> Option.get
  in
  let peers_bytes = peers_string |> Bytes.of_string in
  let peers = create peers_bytes in
  Lwt.return peers

(* let get_sync uri = *)
(*   let open Lwt_result.Syntax in *)
(*   Lwt_main.run *)
(*     (print_endline "Sending request..."; *)
(*      let* response = Piaf.Client.Oneshot.get uri in *)
(*      if Piaf.Status.is_successful response.status *)
(*      then Piaf.Body.to_string response.body *)
(*      else ( *)
(*        let message = Piaf.Status.to_string response.status in *)
(*        Lwt.return (Error (`Msg message)))) *)
(* in *)
(* match get_sync uri with *)
(* | Ok body -> *)
(*   let tracker_bencode = Bencode.decode (`String body) in *)
(*   let peers_string = *)
(*     Bencode_utils.bencode_to_string tracker_bencode "peers" |> Option.get *)
(*   in *)
(*   let peers_bytes = peers_string |> Bytes.of_string in *)
(*   let peers = create peers_bytes in *)
(*   Result.ok peers *)
(* | Error error -> *)
(*   let message = Piaf.Error.to_string error in *)
(*   prerr_endline ("Error: " ^ message); *)
(*   Result.error `Download_peers_error *)
;;

let to_string peer = Printf.sprintf "%s:%d" (V4.to_string peer.ip) peer.port
