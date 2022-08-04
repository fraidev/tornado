type t =
  { conn : string
  ; chocked : bool
  ; bitfield : string
  ; peer : Peers.t
  ; info_hash : string
  ; peer_id : string
  }

(* let create peer peer_id info_hash = *)
(*   let ba = Char.chr 0b11000000 *)
(*   (1* let bb = Stdint.Uint8. ba 0b00100000 *1) *)
(*   let bit = Bytes.set_int8 1 1 *)
(*   let ca = Bytes.set_int8 (Bytes.create 20) 0 0 *)
(*   let a = Bigarray.int8_unsigned 0 0 *)
(*   let c = List.to_seq [ Char.chr 299 ] in *)
(*   let d = Bytes.of_seq c in *)
(*   (1* let conn = Piaf.Client.create Piaf.Config *1) *)
(*   { conn = "BitTorrent protocol" *)
(*   ; chocked = True *)
(*   ; bitfield = "" *)
(*   ; peer *)
(*   ; info_hash *)
(*   ; peer_id *)
(*   } *)
(* ;; *)
