open Lwt.Syntax
open Shared
open Tornado

let handler _socket (ic, oc) =
  let rec loop () =
    let* so = Lwt_io.read_line_opt ic in
    match so with
    | Some s ->
      let* () = Logs_lwt.info (fun m -> m "Client sent: %s" s) in
      let r = String.uppercase_ascii s in
      let* () = Lwt_io.write_line oc r in
      loop ()
    | None ->
      let* () = Logs_lwt.debug (fun m -> m "Client Disconnected.") in
      Lwt.return_unit
  in
  let* () = Logs_lwt.debug (fun m -> m "Client connected!") in
  loop ()
;;

let task_client switch () =
  let* server = Tcp_server.listen handler ~port:8792 in
  Lwt_switch.add_hook (Some switch) (fun () ->
      Tcp_server.close_connection server);
  let* c_conn = Tcp.Client.open_connection "localhost" 8792 in
  let* () = Tcp.Client.write_line c_conn "hey, I am a client\n" in
  let* r = Tcp.Client.read_line c_conn in
  let* () = Tcp.Client.close_connection c_conn.fd in
  Check.check_string "HEY, I AM A CLIENT" r;
  Lwt.return_unit
;;

let tests =
  let open Alcotest_lwt in
  ( "TCP"
  , [ test_case
        "TCP client and server should communicate each other."
        `Quick
        task_client ] )
;;
