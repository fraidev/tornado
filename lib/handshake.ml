type t =
  { pstr : string
  ; info_hash : bytes
  ; peer_id : bytes
  }

let create info_hash peer_id =
  { pstr = "BitTorrent protocol"; info_hash; peer_id }
;;

let serialize h =
  let buf = Buffer.create (String.length h.pstr + 49) in
  Buffer.add_int8 buf (String.length h.pstr);
  Buffer.add_string buf h.pstr;
  Buffer.add_bytes buf (Bytes.make 8 '\000');
  Buffer.add_bytes buf h.info_hash;
  Buffer.add_bytes buf h.peer_id;
  buf
;;
