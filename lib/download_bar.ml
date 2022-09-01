open Progress

let create_bars file_name total =
  let bar color message =
    let open Line.Using_int64 in
    list
      [ rpad 32 (constf " %s" message); bytes; bytes_per_sec
      ; bar ~color ~style:`UTF8 total; percentage_of total ++ const " " ]
      |> Multi.line
  in
  let layout = bar (Color.hex "#90e0ef") file_name in
  with_reporters layout 
;;
