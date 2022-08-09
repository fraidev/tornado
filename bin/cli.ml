open Tornado

let usage = "torreto [-verbose] <torrent_file> -o <output>"
let verbose = ref false
let output_file : string option ref = ref None
let input_files = ref ""
let apply_output_file str = output_file := Some str

let spec_list =
  [ "-verbose", Arg.Set verbose, "Output debug information"
  ; "-o", Arg.String apply_output_file, "Set output file name" ]
;;

(* This func is like a else. 
   When all spec_list are invalid.  *)
let anon_fun filename = input_files := filename
let () = Arg.parse spec_list anon_fun usage

let _ =
  match !output_file with
  | Some file -> print_string file
  | None -> ()
;;

let bencode_file = Bencode.decode (`File_path !input_files);;

let file = Torrent.File.create_with_beencode bencode_file in
let random_peer = Bytes.create 20 in
let uri = Torrent.File.build_tracker_url file random_peer 6881 in
let get_sync uri =
  let open Lwt_result.Syntax in
  Lwt_main.run
    (print_endline "Sending request...";
     let* response = Piaf.Client.Oneshot.get uri in
     if Piaf.Status.is_successful response.status
     then Piaf.Body.to_string response.body
     else (
       let message = Piaf.Status.to_string response.status in
       Lwt.return (Error (`Msg message))))
in
match get_sync uri with
| Ok body ->
  let tracker_bencode = Bencode.decode (`String body) in
  let peers_string =
    Bencode_utils.bencode_to_string tracker_bencode "peers" |> Option.get
  in
  let peers_bytes = peers_string |> Bytes.of_string in
  let peers = Peers.create peers_bytes in
  Printf.printf "Peers: %s\n" (Peers.show peers.(0))
| Error error ->
  let message = Piaf.Error.to_string error in
  prerr_endline ("Error: " ^ message)
