open Lwt.Syntax

module Server = struct
  type conn = { fd : Lwt_unix.file_descr }

  let listen ~port =
    let fd_proto = Lwt_unix.socket PF_INET SOCK_STREAM 0 in
    let* () =
      Lwt_unix.bind fd_proto @@ ADDR_INET (Unix.inet_addr_any, port)
    in
    let () = Lwt_unix.listen fd_proto 5 in
    let* fd, _ = Lwt_unix.accept fd_proto in
    Lwt.return { fd }
  ;;

  let write_line conn s =
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output conn.fd in
    Lwt_io.write oc s
  ;;

  let read_line conn =
    let ic = Lwt_io.of_fd ~mode:Lwt_io.Input conn.fd in
    Lwt_io.read ic
  ;;

  let close_connection fd = Lwt_unix.close fd
end

module Client = struct
  type conn =
    { fd : Lwt_unix.file_descr
    ; ch_in : Lwt_io.input_channel
    ; ch_out : Lwt_io.output_channel
    }

  let close_connection fd = Lwt_unix.close fd

  let open_connection hostname port =
    let fd = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    let server_addr = (Unix.gethostbyname hostname).Unix.h_addr_list.(0) in
    let sockaddr = Unix.ADDR_INET (server_addr, port) in
    let* ch_in, ch_out = Lwt_io.open_connection ~fd sockaddr in
    Lwt.return { fd; ch_in; ch_out }
  ;;

  let write_line conn s =
    let oc = Lwt_io.of_fd ~mode:Lwt_io.Output conn.fd in
    Lwt_io.write oc s
  ;;

  let read_line conn =
    let* r = Lwt_io.read_line conn.ch_in in
    let* () = Lwt_io.(write stdout) ("Client received: " ^ r ^ "\n") in
    Lwt.return r
  ;;
end
