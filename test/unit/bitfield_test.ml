open Tornado
open Shared

type case =
  { actual : char array
  ; expected : char array
  ; index : int
  }

let has_piece_test () =
  let bf =
    Bitfield.of_char_array [| Char.chr 0b01010100; Char.chr 0b01010100 |]
  in
  let outputs =
    [ false; true; false; true; false; true; false; false; false; true; false
    ; true; false; true; false; false; false; false; false; false ]
  in
  List.iteri
    (fun index expected ->
      Check.check_bool expected (Bitfield.has_piece bf index))
    outputs
;;

let set_piece_test () =
  let cases =
    [ (* Set *)
      { actual = [| Char.chr 0b01010100; Char.chr 0b01010100 |]
      ; expected = [| Char.chr 0b01011100; Char.chr 0b01010100 |]
      ; index = 4
      }; (* Not Set *)
      { actual = [| Char.chr 0b01010100; Char.chr 0b01010100 |]
      ; expected = [| Char.chr 0b01010100; Char.chr 0b01010100 |]
      ; index = 9
      }
      (* Set *)
    ; { actual = [| Char.chr 0b01010100; Char.chr 0b01010100 |]
      ; expected = [| Char.chr 0b01010100; Char.chr 0b01010101 |]
      ; index = 15
      }
      (* Not Set *)
    ; { actual = [| Char.chr 0b01010100; Char.chr 0b01010100 |]
      ; expected = [| Char.chr 0b01010100; Char.chr 0b01010100 |]
      ; index = 19
      } ]
  in
  List.iter
    (fun case ->
      let actual_b = Bitfield.of_char_array case.actual in
      let expected_b = Bitfield.of_char_array case.expected in
      Bitfield.set_piece actual_b case.index;
      Check.check_array_char expected_b actual_b)
    cases
;;

let tests =
  let open Alcotest in
  ( "Bitfield"
  , [ test_case "Should set a bit on selected index." `Quick set_piece_test
    ; test_case
        "Should return when there is a piece true by index."
        `Quick
        has_piece_test ] )
;;
