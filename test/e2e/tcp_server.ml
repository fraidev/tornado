let close_connection server = Lwt_io.shutdown_server server

let listen handler ~port =
  let socketaddr = Unix.ADDR_INET (Unix.inet_addr_loopback, port) in
  Lwt_io.establish_server_with_client_address socketaddr handler
;;
