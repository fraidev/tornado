(* open Lwt.Syntax *)

module Client = struct
  let open_connection host port =
    let client = `TCP (`IP Ipaddr.(V4 host), `Port port) in
    Conduit_lwt_unix.(connect ~ctx:(Lazy.force default_ctx) client)
  ;;

  let write_bytes oc buf =
    let buf_len = Bytes.length buf in
    Lwt_io.write_from_exactly oc buf 0 buf_len
  ;;
  (* let* () = Lwt_io.write_from_exactly oc buf 0 buf_len in *)
  (* Lwt_io.flush oc *)
end
