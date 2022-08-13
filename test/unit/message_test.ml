open Shared
open Tornado
open Stdint

let format_request_test () =
  let expected =
    [ 0x00; 0x00; 0x00; 0x04; (* Index *) 0x00; 0x00; 0x02; 0x37; (* Begin *)
      0x00; 0x00; 0x10; 0xe1 (* Length *) ]
    |> Utils.ints_to_bytes
  in
  let index = Uint32.of_int 4 in
  let start = Uint32.of_int 567 in
  let length = Uint32.of_int 4321 in
  let msg = Message.format_request index start length in
  Check.check_bytes expected msg.payload
;;

let format_have_test () =
  let expected = [ 0x00; 0x00; 0x00; 0x04 ] |> Utils.ints_to_bytes in
  let index = Uint32.of_int 4 in
  let msg = Message.format_have index in
  Check.check_bytes expected msg.payload
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
    ; test_case "Format have." `Quick format_have_test ]
    @ to_string_tests )
;;
