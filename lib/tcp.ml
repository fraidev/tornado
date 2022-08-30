module Client = struct
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

  let read socket_flow l = Bytes.to_string (read_bytes socket_flow l)
end
