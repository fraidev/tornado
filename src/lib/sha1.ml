let nybble_to_hex_nybble n =
  if n >= 0 && n < 10
  then Char.chr (n + 48)
  else if n < 16
  then Char.chr (n + 97 - 10)
  else failwith (Printf.sprintf "Illegal hex nybble value: %d" n)
;;

let byte_to_hex_bytes b =
  let l = b lsr 4 in
  let r = b land 15 in
  nybble_to_hex_nybble l, nybble_to_hex_nybble r
;;

let encode_hex (src : bytes) : bytes =
  let open Bytes in
  let dst = create (length src * 2) in
  for i = 0 to length src - 1 do
    let l, r = byte_to_hex_bytes (Char.code (get src i)) in
    set dst (i * 2) l;
    set dst ((i * 2) + 1) r
  done;
  dst
;;

(* Left-rotate *)
let rol (value : int) (bits : int) : int =
  (value lsl bits) lor (value lsr (32 - bits)) land 0xffffffff
;;

(* Number to its 32-bit big-endian representation *)
let number_to_32_bit_be (n : int) : bytes =
  let open Bytes in
  let result = create 4 in
  set result 0 (Char.chr ((n land 0xff000000) lsr 24));
  set result 1 (Char.chr ((n land 0x00ff0000) lsr 16));
  set result 2 (Char.chr ((n land 0x0000ff00) lsr 8));
  set result 3 (Char.chr (n land 0x000000ff));
  result
;;

(* 32-bit big-endian representation of a number *)
let number_of_32_bit_be (b : bytes) : int =
  let open Bytes in
  (Char.code (get b 0) lsl 24)
  + (Char.code (get b 1) lsl 16)
  + (Char.code (get b 2) lsl 8)
  + Char.code (get b 3)
;;

(* SHA-1 digest *)
let digest (bytes : bytes) : bytes =
  let h0 = ref 0x67452301 in
  let h1 = ref 0xEFCDAB89 in
  let h2 = ref 0x98BADCFE in
  let h3 = ref 0x10325476 in
  let h4 = ref 0xC3D2E1F0 in
  let ml = Bytes.length bytes * 8 in
  let preprocess _a =
    let length_mod_512 = ml mod 512 in
    let length_to_add =
      if 512 - length_mod_512 >= 65
      then 512 - length_mod_512
      else 512 - length_mod_512 + 512
    in
    let to_append = Bytes.make (length_to_add / 8) '\x00' in
    Bytes.set to_append 0 '\x80';
    Bytes.blit
      (number_to_32_bit_be ml)
      0
      to_append
      (Bytes.length to_append - 4)
      4;
    Bytes.concat (Bytes.of_string "") [ _a; to_append ]
  in
  let preprocessed = preprocess bytes in
  let preprocessed_length = Bytes.length preprocessed in
  assert (preprocessed_length mod 64 = 0);
  for i = 0 to (preprocessed_length / 64) - 1 do
    let chunk = Bytes.sub preprocessed (i * 64) 64 in
    let w =
      Array.init 80 (fun k ->
        if k < 16 then number_of_32_bit_be (Bytes.sub chunk (k * 4) 4) else 0)
    in
    for j = 16 to 79 do
      w.(j)
        <- rol (w.(j - 3) lxor w.(j - 8) lxor w.(j - 14) lxor w.(j - 16)) 1
    done;
    let a = ref !h0 in
    let b = ref !h1 in
    let c = ref !h2 in
    let d = ref !h3 in
    let e = ref !h4 in
    let temp = ref 0 in
    let k = ref 0 in
    let f = ref 0 in
    (* main loop *)
    for j = 0 to 79 do
      if 0 <= j && j < 20
      then (
        f := !b land !c lxor (lnot !b land !d);
        k := 0x5A827999)
      else if 20 <= j && j < 40
      then (
        f := !b lxor !c lxor !d;
        k := 0x6ED9EBA1)
      else if 40 <= j && j < 60
      then (
        f := !b land !c lor (!b land !d) lor (!c land !d);
        k := 0x8F1BBCDC)
      else (
        f := !b lxor !c lxor !d;
        k := 0xCA62C1D6);
      temp := (rol !a 5 + !f + !e + !k + w.(j)) mod (1 lsl 32);
      e := !d;
      d := !c;
      c := rol !b 30;
      b := !a;
      a := !temp
    done;
    h0 := (!h0 + !a) mod (1 lsl 32);
    h1 := (!h1 + !b) mod (1 lsl 32);
    h2 := (!h2 + !c) mod (1 lsl 32);
    h3 := (!h3 + !d) mod (1 lsl 32);
    h4 := (!h4 + !e) mod (1 lsl 32)
  done;
  Bytes.concat
    (Bytes.of_string "")
    (List.map number_to_32_bit_be [ !h0; !h1; !h2; !h3; !h4 ])
;;
