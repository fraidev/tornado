
let listen ~net ~sw ~port =
  let addr = `Tcp (Eio.Net.Ipaddr.V4.loopback, port) in
  let socket = Eio.Net.listen net ~sw ~reuse_addr:true ~backlog:5 addr in
  let flow, addr = Eio.Net.accept ~sw socket in
  flow, addr
;;
