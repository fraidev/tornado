open Tornado

let usage = "tornado [--verbose] <torrent_file> -o <output>"
let verbose = ref false
let output_file : string option ref = ref None
let input_file = ref ""
let apply_output_file str = output_file := Some str

let spec_list =
  [ "--verbose", Arg.Set verbose, "Output debug information"
  ; "-o", Arg.String apply_output_file, "Set output file name" ]
;;

(* This func is like a else. 
   When all spec_list are invalid.  *)
let anon_fun filename = input_file := filename

let () =
  Arg.parse spec_list anon_fun usage;
  Log.setup_log (Some (if !verbose then Debug else App));
  let torrent_file = Torrent_file.open_file !input_file in
  Eio_main.run
  @@ fun env -> Torrent_client.download_file !output_file torrent_file env
;;
