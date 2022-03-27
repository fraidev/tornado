let err_msg = "torreto [-verbose] <torrent_file> -o <output>"
let verbose = ref false
let output_file : string option ref = ref None
let input_files = ref ""
let anon_fun filename = input_files := filename
let apply_output_file str = output_file := Some str

let speclist =
  [ "-verbose", Arg.Set verbose, "Output debug information"
  ; "-o", Arg.String apply_output_file, "Set output file name"
  ]
;;

let () =
  Arg.parse speclist anon_fun err_msg;
  Core.print_string !input_files
  let _ =
    match !output_file with
    | Some file -> Core.print_string file
    | None -> ()
;;
