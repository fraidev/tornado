type t = bytes

let of_bytes (input : bytes) : t = input

let has_piece (bf : t) index =
  let byteIndex = index / 8 in
  let offset = index mod 8 in
  if byteIndex < 0 || byteIndex >= Bytes.length bf
  then false
  else (Char.code (Bytes.get bf byteIndex) lsr (7 - offset)) land 1 <> 0
;;

let set_piece (bf : t) index =
  let byteIndex = index / 8 in
  let offset = index mod 8 in
  if byteIndex < 0 || byteIndex >= Bytes.length bf
  then ()
  else (
    let newChar =
      Char.chr (Char.code (Bytes.get bf byteIndex) lor (1 lsl (7 - offset)))
    in
    Bytes.set bf byteIndex newChar)
;;
