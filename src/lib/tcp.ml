module Client = struct
  let open_connection
    ~(env : Eio_unix.Stdenv.base)
    ~sw
    ~(host : Ipaddr.V4.t)
    ~port
    =
    let ip = Ipaddr.V4.to_octets host in
    let addr = `Tcp (Eio.Net.Ipaddr.of_raw ip, port) in
    let net = Eio.Stdenv.net env in
    let flow = (Eio.Net.connect ~sw net addr :> Eio.Flow.two_way) in
    let fd = Eio_unix.Resource.fd_opt flow |> Option.get in
    Eio_unix.Fd.use_exn "Client.open_connection" fd (fun fd ->
      Unix.setsockopt fd TCP_NODELAY false;
      Logs.debug (fun m -> m "TCP_NODELAY Disabled"));
    flow
  ;;

  let write_bytes socket_flow buf =
    Eio.Buf_write.with_flow socket_flow (fun buf_write ->
      Eio.Buf_write.bytes buf_write buf)
  ;;

  let write socket_flow str = Eio.Flow.copy_string str socket_flow

  let read_bytes socket_flow size =
    let buf = Cstruct.create size in
    Eio.Flow.read_exact socket_flow buf;
    Cstruct.to_bytes buf
  ;;

  let read socket_flow size = Bytes.to_string (read_bytes socket_flow size)
end
