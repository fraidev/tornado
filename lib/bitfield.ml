type t = char array

let of_char_array (input : char array) : t = input

let has_piece (bf : t) index =
  let byteIndex = index / 8 in
  let offset = index mod 8 in
  if byteIndex < 0 || byteIndex >= Array.length bf
  then false
  else (Char.code bf.(byteIndex) lsr (7 - offset)) land 1 <> 0
;;

let set_piece (bf : t) index =
  let byteIndex = index / 8 in
  let offset = index mod 8 in
  if byteIndex < 0 || byteIndex >= Array.length bf
  then ()
  else (
    let newChar =
      Char.chr (Char.code bf.(byteIndex) lor (1 lsl (7 - offset)))
    in
    Array.set bf byteIndex newChar)
;;
