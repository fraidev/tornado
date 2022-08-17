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

let info fmt a = Logs.info (fun m -> m fmt a)
let debug fmt a = Logs.debug (fun m -> m fmt a)
let err fmt a = Logs.err (fun m -> m fmt a)
let info_lwt fmt a = Logs_lwt.info (fun m -> m fmt a)
let debug_lwt fmt a = Logs_lwt.debug (fun m -> m fmt a)
let err_lwt fmt a = Logs_lwt.err (fun m -> m fmt a)
