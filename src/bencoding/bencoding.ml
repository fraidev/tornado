let decode input =
  print_string input;
  "a"
;; 

(* type bencodes = bencodes bencode *)

(* let bencode = function input 
  | Bencode_int Int
  | Bencode_string String
  | Bencode_list [bencode_value]
  | Bencode_dict [(String, bencode_value)] *)

let students = [ 1, "abc" ]

(* type bencode_value =
  | Bencode_int of int
  | Bencode_string of string
  | Bencode_bytes of bytes
  | Bencode_list of bencode_value list
  | Bencode_dict of (string * bencode_value) list *)

(* type bencode =
  [ `Bencode_int of int
  | `Bencode_string of string
  | `Bencode_bytes of bytes
  | `Bencode_list of bencode list
  | `Bencode_dict of (string * bencode) list
  ] *)

(* 
type bencode =
  | Key_value of (string * bencode_value)
  | Key_object of bencode list *)

(* let bencode_example : bencode =
  `Bencode_list
    [ Key_value
        ( "announce"
        , Bencode_string "http://bttracker.debian.org:6969/announce" )
    ; Key_value ("comment", Bencode_string "Debian CD from cdimage.debian.org")
    ; Key_value ("creation date", Bencode_int 1573903810)
    ; Key_object
        [ Key_value ("length date", Bencode_int 351272960)
        ; Key_value ("name", Bencode_string "debian-9.5.0-amd64-netinst.iso")
        ; Key_value ("piece length", Bencode_int 262144)
        ; Key_value ("pieces", Bencode_bytes (Bytes.of_string "�����PS�^��"))
        ]
    ; Key_value ("c", Bencode_list [ Bencode_int 1; Bencode_string "abc" ])
    ; Key_value
        ("", Bencode_dict [ "a", Bencode_int 1; "b", Bencode_string "abc" ])
    ]
;; *)

(* let b = List.nth bencode_example 3 *)
