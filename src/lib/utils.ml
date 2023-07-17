let ints_to_bytes list =
  list |> List.map (fun i -> Char.chr i) |> List.to_seq |> Bytes.of_seq
;;
