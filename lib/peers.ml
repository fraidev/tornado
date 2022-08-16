open Stdint
open Ipaddr

type t =
  { ip : V4.t
  ; port : int
  }
[@@deriving show]

let create peers_bin =
  let peer_size = 6 in
  let peers_bin_length = Bytes.length peers_bin in
  Printf.printf "peers_bin_length: %d\n" peers_bin_length;
  let num_peers = peers_bin_length / peer_size in
  Printf.printf "num_peers: %d\n" num_peers;
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

let to_string peer = Printf.sprintf "%s:%d" (V4.to_string peer.ip) peer.port
