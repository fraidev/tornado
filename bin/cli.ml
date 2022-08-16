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

let setup_log ?style_renderer level =
  let pp_header src ppf (l, h) =
    if l = Logs.App
    then Format.fprintf ppf "%a" Logs_fmt.pp_header (l, h)
    else (
      let x =
        match Array.length Sys.argv with
        | 0 -> Filename.basename Sys.executable_name
        | _n -> Filename.basename Sys.argv.(0)
      in
      let x =
        if Logs.Src.equal src Logs.default then x else Logs.Src.name src
      in
      Format.fprintf ppf "%s: %a " x Logs_fmt.pp_header (l, h))
  in
  let format_reporter =
    let report src =
      let { Logs.report } = Logs_fmt.reporter ~pp_header:(pp_header src) () in
      report src
    in
    { Logs.report }
  in
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level ~all:true level;
  Logs.set_reporter format_reporter
;;

(* This func is like a else. 
   When all spec_list are invalid.  *)
let anon_fun filename = input_file := filename

let () =
  setup_log (Some Debug);
  Arg.parse spec_list anon_fun usage;
  let torrent_file = Torrent_file.open_file !input_file in
  Lwt_main.run (Torrent_file.download_file !output_file torrent_file)
;;
