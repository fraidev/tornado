open Shared
open Tornado

let format_request_test () =
  let expected =
    [ 0x00; 0x00; 0x00; 0x04; (* Index *) 0x00; 0x00; 0x02; 0x37; (* Begin *)
      0x00; 0x00; 0x10; 0xe1 (* Length *) ] |> Utils.ints_to_bytes
  in
  let index = Int32.of_int 4 in
  let start = Int32.of_int 567 in
  let length = Int32.of_int 4321 in
  let msg = Message.format_request index start length in
  Check.check_bytes expected msg.payload
;;

let tests =
  let open Alcotest in
  "Message", [ test_case "Format request." `Quick format_request_test ]
;;
