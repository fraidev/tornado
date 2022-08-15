open Tornado
open Shared

type case =
  { actual : bytes
  ; expected : bytes
  ; index : int
  }

let has_piece_test () =
  let bf =
    [ 0b01010100; 0b01010100 ] |> Utils.ints_to_bytes |> Bitfield.of_bytes
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
      { actual = [ 0b01010100; 0b01010100 ] |> Utils.ints_to_bytes
      ; expected = [ 0b01011100; 0b01010100 ] |> Utils.ints_to_bytes
      ; index = 4
      }; (* Not Set *)
      { actual = [ 0b01010100; 0b01010100 ] |> Utils.ints_to_bytes
      ; expected = [ 0b01010100; 0b01010100 ] |> Utils.ints_to_bytes
      ; index = 9
      }
      (* Set *)
    ; { actual = [ 0b01010100; 0b01010100 ] |> Utils.ints_to_bytes
      ; expected = [ 0b01010100; 0b01010101 ] |> Utils.ints_to_bytes
      ; index = 15
      }
      (* Not Set *)
    ; { actual = [ 0b01010100; 0b01010100 ] |> Utils.ints_to_bytes
      ; expected = [ 0b01010100; 0b01010100 ] |> Utils.ints_to_bytes
      ; index = 19
      } ]
  in
  List.iter
    (fun case ->
      let actual_b = Bitfield.of_bytes case.actual in
      let expected_b = Bitfield.of_bytes case.expected in
      Bitfield.set_piece actual_b case.index;
      Check.check_bytes expected_b actual_b)
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
