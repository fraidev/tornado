open Lwt.Syntax

let skip _ = ()

let listen ~stop ~port handler =
  let config = `Port port in
  let* ctx = Conduit_lwt_unix.init ~src:"127.0.0.1" () in
  let server () =
    Conduit_lwt_unix.serve ~stop ~ctx ~mode:(`TCP config) ~on_exn:skip handler
  in
  Lwt.return server
;;
