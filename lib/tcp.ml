open Lwt.Syntax

module Client = struct
  (* type conn = { fd : Lwt_unix.file_descr } *)

  (* let input_channel conn = Lwt_io.of_fd ~mode:Lwt_io.Input conn.fd *)
  (* let output_channel conn = Lwt_io.of_fd ~mode:Lwt_io.Output conn.fd *)

  let open_connection port =
    let client = `TCP (`IP Ipaddr.(V4 V4.localhost), `Port port) in
    Conduit_lwt_unix.(connect ~ctx:(Lazy.force default_ctx) client)
  ;;

  let write_bytes oc buf =
    (* let oc = output_channel conn in *)
    let buf_len = Bytes.length buf in
    let* () = Lwt_io.write_from_exactly oc buf 0 buf_len in
    Lwt_io.flush oc
  ;;

  let write_line oc s =
    (* let oc = output_channel conn in *)
    let* () = Lwt_io.write_line oc s in
    Lwt_io.flush oc
  ;;

  let read_line ic =
    (* let ic = input_channel conn in *)
    let* r = Lwt_io.read_line ic in
    Lwt.return r
  ;;
end
