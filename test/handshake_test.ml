open Lwt.Syntax
open Tornado

let seq_test () =
  let hash_info =
    [ 134; 212; 200; 0; 36; 164; 105; 190; 76; 80; 188; 90; 16; 44; 247; 23
    ; 128; 49; 0; 116 ]
    |> Utils.ints_to_bytes
  in
  let peer_id =
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20 ]
    |> Utils.ints_to_bytes
  in
  let expected =
    [ 19; 66; 105; 116; 84; 111; 114; 114; 101; 110; 116; 32; 112; 114; 111
    ; 116; 111; 99; 111; 108; 0; 0; 0; 0; 0; 0; 0; 0; 134; 212; 200; 0; 36
    ; 164; 105; 190; 76; 80; 188; 90; 16; 44; 247; 23; 128; 49; 0; 116; 1; 2
    ; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20 ]
    |> Utils.ints_to_bytes
  in
  let handshake = Handshake.create hash_info peer_id in
  let buf = Handshake.serialize handshake in
  let result = Buffer.to_bytes buf in
  Check.check_bytes expected result
;;

type case =
  { input : bytes
  ; expected : Handshake.t option
  ; error : Handshake.erros option
  }

let read_test_task () =
  let hash_info =
    [ 134; 212; 200; 0; 36; 164; 105; 190; 76; 80; 188; 90; 16; 44; 247; 23
    ; 128; 49; 0; 116 ]
    |> Utils.ints_to_bytes
  in
  let peer_id =
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20 ]
    |> Utils.ints_to_bytes
  in
  let input =
    [ 19; 66; 105; 116; 84; 111; 114; 114; 101; 110; 116; 32; 112; 114; 111
    ; 116; 111; 99; 111; 108; 0; 0; 0; 0; 0; 0; 0; 0; 134; 212; 200; 0; 36
    ; 164; 105; 190; 76; 80; 188; 90; 16; 44; 247; 23; 128; 49; 0; 116; 1; 2
    ; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20 ]
    |> Utils.ints_to_bytes
  in
  let cases =
    [ { input
      ; expected = Option.some (Handshake.create hash_info peer_id)
      ; error = Option.none
      }
    ; { input = [ 0; 0; 0 ] |> Utils.ints_to_bytes
      ; expected = Option.none
      ; error = Option.some `Pstrlen_cannot_be_zero
      } ]
  in
  Lwt_list.iter_p
    (fun case ->
      let ch_in =
        Lwt_io.of_bytes ~mode:Lwt_io.Input (Lwt_bytes.of_bytes case.input)
      in
      let* handshake = Handshake.read ch_in in
      (match case.expected with
      | Some exp ->
        let result_handshake = Result.get_ok handshake in
        Check.check_string exp.pstr result_handshake.pstr;
        Check.check_bytes exp.info_hash result_handshake.info_hash;
        Check.check_bytes exp.peer_id result_handshake.peer_id
      | None ->
        let result_handshake = Result.get_error handshake in
        let same_error = result_handshake == Option.get case.error in
        Check.check_bool true same_error);
      Lwt.return ())
    cases
;;

let read_test () = Lwt_main.run (read_test_task ())

let tests =
  let open Alcotest in
  ( "Handshake"
  , [ test_case "Serialize handshake." `Quick seq_test
    ; test_case "Read handshake." `Quick read_test ] )
;;
