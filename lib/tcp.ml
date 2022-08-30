open Eio

module Client = struct
  let run_server socket =
    Switch.run
    @@ fun sw ->
    Eio.Net.accept_fork
      socket
      ~sw
      (fun flow _addr ->
        traceln "Server accepted connection from client";
        let b = Buffer.create 100 in
        Eio.Flow.copy flow (Eio.Flow.buffer_sink b);
        traceln "Server received: %S" (Buffer.contents b))
      ~on_error:(traceln "Error handling connection: %a" Fmt.exn);
    traceln "(normally we'd loop and accept more connections here)"
  ;;

  let open_connection (host : Ipaddr.V4.t) port (env : Eio.Stdenv.t) sw =
    let ip = Ipaddr.V4.to_octets host in
    let addr = `Tcp (Eio.Net.Ipaddr.of_raw ip, port) in
    Eio.Net.connect ~sw (Eio.Stdenv.net env) addr
  ;;

  let write_bytes socket_flow buf =
    Eio.Buf_write.with_flow socket_flow (fun buf_write ->
      Eio.Buf_write.bytes buf_write buf)
  ;;

  let read_bytes socket_flow l =
    let buf = Cstruct.create l in
    Eio.Flow.read_exact socket_flow buf;
    Cstruct.to_bytes buf
  ;;

  (* let buf_read = Eio.Buf_read.of_flow ~max_size:1_000_000 socket_flow in *)
  (* Eio.Buf_read.take l buf_read *)

  (* Eio.Buf_read.line buf_read *)

  let read socket_flow l = Bytes.to_string (read_bytes socket_flow l)
end
