open Lwt.Syntax
open Tornado

let seq_test () =
  let hash_info =
    [ 134; 212; 200; 0; 36; 164; 105; 190; 76; 80; 188; 90; 16; 44; 247; 23
    ; 128; 49; 0; 116 ]
    |> List.map (fun i -> Char.chr i)
    |> List.to_seq
    |> Bytes.of_seq
  in
  let peer_id =
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20 ]
    |> List.map (fun i -> Char.chr i)
    |> List.to_seq
    |> Bytes.of_seq
  in
  let expected =
    [ 19; 66; 105; 116; 84; 111; 114; 114; 101; 110; 116; 32; 112; 114; 111
    ; 116; 111; 99; 111; 108; 0; 0; 0; 0; 0; 0; 0; 0; 134; 212; 200; 0; 36
    ; 164; 105; 190; 76; 80; 188; 90; 16; 44; 247; 23; 128; 49; 0; 116; 1; 2
    ; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20 ]
    |> List.map (fun i -> Char.chr i)
    |> Array.of_list
  in
  let handshake = Handshake.create hash_info peer_id in
  let buf = Handshake.serialize handshake in
  let r = Buffer.to_seq buf |> Array.of_seq in
  Check.check_array_char expected r
;;

let read_test_task () =
  let hash_info =
    [ 134; 212; 200; 0; 36; 164; 105; 190; 76; 80; 188; 90; 16; 44; 247; 23
    ; 128; 49; 0; 116 ]
    |> List.map (fun i -> Char.chr i)
    |> List.to_seq
    |> Bytes.of_seq
  in
  let peer_id =
    [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20 ]
    |> List.map (fun i -> Char.chr i)
    |> List.to_seq
    |> Bytes.of_seq
  in
  let input =
    [ 19; 66; 105; 116; 84; 111; 114; 114; 101; 110; 116; 32; 112; 114; 111
    ; 116; 111; 99; 111; 108; 0; 0; 0; 0; 0; 0; 0; 0; 134; 212; 200; 0; 36
    ; 164; 105; 190; 76; 80; 188; 90; 16; 44; 247; 23; 128; 49; 0; 116; 1; 2
    ; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20 ]
    |> List.map (fun i -> Char.chr i)
    |> List.to_seq
    |> Bytes.of_seq
  in
  let handshake = Handshake.create hash_info peer_id in
  let ch_in = Lwt_io.of_bytes ~mode:Lwt_io.Input (Lwt_bytes.of_bytes input) in
  let* result_handshake = Handshake.read ch_in in
  Check.check_string handshake.pstr result_handshake.pstr;
  Check.check_bytes handshake.info_hash result_handshake.info_hash;
  Check.check_bytes handshake.peer_id result_handshake.peer_id;
  Lwt.return ()
;;

let read_test () = Lwt_main.run (read_test_task ())

let tests =
  let open Alcotest in
  ( "Handshake"
  , [ test_case "Serialize handshake." `Quick seq_test
    ; test_case "Read handshake." `Quick read_test ] )
;;
