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
    match Eio.Net.connect ~sw net addr with
    | socket_flow ->
      let fd = Eio_unix.Resource.fd_opt socket_flow |> Option.get in
      Eio_unix.Fd.use_exn "Client.open_connection" fd (fun fd ->
        Unix.setsockopt fd TCP_NODELAY false;
        Logs.debug (fun m -> m "TCP_NODELAY Disabled"));
      (socket_flow :> Eio.Flow.two_way_ty Eio.Flow.two_way)
    | exception exn ->
      Logs.err (fun m ->
        m
          "Failed to connect to %a:%d: %s"
          Ipaddr.V4.pp
          host
          port
          (Printexc.to_string exn));
      raise exn
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
