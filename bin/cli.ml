open Tornado

let usage = "torreto [-verbose] <torrent_file> -o <output>"
let verbose = ref false
let output_file : string option ref = ref None
let input_file = ref ""
let apply_output_file str = output_file := Some str

let spec_list =
  [ "-verbose", Arg.Set verbose, "Output debug information"
  ; "-o", Arg.String apply_output_file, "Set output file name" ]
;;

(* This func is like a else. 
   When all spec_list are invalid.  *)
let anon_fun filename = input_file := filename

let () =
  Log.setup_log (Some Debug);
  Arg.parse spec_list anon_fun usage;
  let torrent_file = Torrent_file.open_file !input_file in
  Lwt_main.run (Torrent_file.download_file !output_file torrent_file)
;;
