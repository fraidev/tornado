open Lwt.Syntax

module Client = struct
  type conn = { fd : Lwt_unix.file_descr }

  let input_channel conn = Lwt_io.of_fd ~mode:Lwt_io.Input conn.fd
  let output_channel conn = Lwt_io.of_fd ~mode:Lwt_io.Output conn.fd
  let close_connection fd = Lwt_unix.close fd

  let open_connection hostname port =
    let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let server_addr = (Unix.gethostbyname hostname).Unix.h_addr_list.(0) in
    let sockaddr = Lwt_unix.ADDR_INET (server_addr, port) in
    let* () = Lwt_unix.connect fd sockaddr in
    Lwt.return { fd }
  ;;

  let write_line conn s =
    let oc = output_channel conn in
    Lwt_io.write_line oc s
  ;;

  let read_line conn =
    let ic = input_channel conn in
    let* r = Lwt_io.read_line ic in
    Lwt.return r
  ;;
end
