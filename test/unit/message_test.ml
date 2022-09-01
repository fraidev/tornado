open Shared
open Tornado
open Stdint

let format_request_test () =
  let expected =
    [ 0x00; 0x00; 0x00; 0x04; (* Index *) 0x00; 0x00; 0x02; 0x37; (* Begin *)
      0x00; 0x00; 0x10; 0xe1 (* Length *) ]
    |> Utils.ints_to_bytes
  in
  let index = 4 in
  let start = 567 in
  let length = 4321 in
  let msg = Message.format_request index start length in
  Check.check_bytes expected msg.payload
;;

let format_have_test () =
  let expected = [ 0x00; 0x00; 0x00; 0x04 ] |> Utils.ints_to_bytes in
  let index = Uint32.of_int 4 in
  let msg = Message.format_have index in
  Check.check_bytes expected msg.payload
;;

(* TODO FAILURE TESTS *)
let parse_piece_test () =
  let index = 4 in
  let buf = Bytes.make 10 '\000' in
  let msg : Message.t =
    { id = Message.id_of_message_type Msg_piece
    ; payload =
        [ 0x00; 0x00; 0x00; 0x04; (* Index *) 0x00; 0x00; 0x00; 0x02
        ; (* Begin *) 0xaa; 0xbb; 0xcc; 0xdd; 0xee; 0xff (* Length *) ]
        |> Utils.ints_to_bytes
    }
  in
  let expected_buf =
    [ 0x00; 0x00; 0xaa; 0xbb; 0xcc; 0xdd; 0xee; 0xff; 0x00; 0x00 ]
    |> Utils.ints_to_bytes
  in
  let expected_n = 6 in
  let n = Result.get_ok (Message.parse_piece index buf msg) in
  Check.check_bytes expected_buf buf;
  Check.check_int expected_n n
;;

(* TODO FAILURE TESTS *)
let parse_have_test () =
  let msg : Message.t =
    { id = Message.id_of_message_type Msg_have
    ; payload = [ 0x00; 0x00; 0x00; 0x04 ] |> Utils.ints_to_bytes
    }
  in
  let expected = 4 in
  let result = Result.get_ok (Message.parse_have msg) in
  Check.check_int expected result
;;

type to_string_case =
  { input : Message.t option
  ; expected : string
  }

let to_string_tests =
  let cases =
    [ { input =
          Some
            { id = Uint8.of_int 0
            ; payload = [ 1; 2; 3 ] |> Utils.ints_to_bytes
            }
      ; expected = "Choke [3]"
      }
    ; { input =
          Some
            { id = Uint8.of_int 1
            ; payload = [ 1; 2; 3 ] |> Utils.ints_to_bytes
            }
      ; expected = "Unchoke [3]"
      }
    ; { input =
          Some
            { id = Uint8.of_int 2
            ; payload = [ 1; 2; 3 ] |> Utils.ints_to_bytes
            }
      ; expected = "Interested [3]"
      }
    ; { input =
          Some
            { id = Uint8.of_int 3
            ; payload = [ 1; 2; 3 ] |> Utils.ints_to_bytes
            }
      ; expected = "NotInterested [3]"
      }
    ; { input =
          Some
            { id = Uint8.of_int 4
            ; payload = [ 1; 2; 3 ] |> Utils.ints_to_bytes
            }
      ; expected = "Have [3]"
      }
    ; { input =
          Some
            { id = Uint8.of_int 5
            ; payload = [ 1; 2; 3 ] |> Utils.ints_to_bytes
            }
      ; expected = "Bitfield [3]"
      }
    ; { input =
          Some
            { id = Uint8.of_int 6
            ; payload = [ 1; 2; 3 ] |> Utils.ints_to_bytes
            }
      ; expected = "Request [3]"
      }
    ; { input =
          Some
            { id = Uint8.of_int 7
            ; payload = [ 1; 2; 3 ] |> Utils.ints_to_bytes
            }
      ; expected = "Piece [3]"
      }
    ; { input =
          Some
            { id = Uint8.of_int 8
            ; payload = [ 1; 2; 3 ] |> Utils.ints_to_bytes
            }
      ; expected = "Cancel [3]"
      }
    ; { input =
          Some
            { id = Uint8.of_int 99
            ; payload = [ 1; 2; 3 ] |> Utils.ints_to_bytes
            }
      ; expected = "Unknown#99 [3]"
      }; { input = None; expected = "KeepAlive" } ]
  in
  List.map
    (fun c ->
      Alcotest.test_case
        ("To string message: " ^ c.expected)
        `Quick
        (fun () ->
        let msg_str = Message.to_string c.input in
        Check.check_string c.expected msg_str))
    cases
;;

let tests =
  let open Alcotest in
  ( "Message"
  , [ test_case "Format request." `Quick format_request_test
    ; test_case "Format have." `Quick format_have_test
    ; test_case "Parse piece." `Quick parse_piece_test
    ; test_case "Parse have." `Quick parse_have_test ]
    @ to_string_tests )
;;
