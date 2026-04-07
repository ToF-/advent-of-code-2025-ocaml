let read_lines file_name =
  let lines =
    String.split_on_char '\n'
      (In_channel.with_open_bin file_name In_channel.input_all)
  in
  List.filter (fun s -> s <> "") lines

let dial instruction =
  if String.length instruction < 2 then invalid_arg "instruction too short"
  else
    let value =
      int_of_string (String.sub instruction 1 (String.length instruction - 1))
    in
    let sign = String.sub instruction 0 1 in
    match sign with "L" -> -value | _ -> value

let rec ticks_a accum pos values =
  match values with
  | [] -> accum
  | x :: xs -> (
      let new_pos = pos + x in
      match new_pos mod 100 == 0 with
      | true -> ticks_a (accum + 1) new_pos xs
      | false -> ticks_a accum new_pos xs)

let actual_password_a file_name =
  let lines = read_lines file_name in
  let values = List.map dial lines in
  ticks_a 0 50 values

let modulo x y =
  let r = x mod y in
  if r >= 0 then r else r + y

let clicks pos delta =
  if delta >= 0 then (pos + delta) / 100 else (modulo (-pos) 100 - delta) / 100

let rec ticks_b counter pos values =
  match values with
  | [] -> counter
  | x :: xs ->
      let new_pos = modulo (pos + x) 100 in
      ticks_b (counter + clicks pos x) new_pos xs

let actual_password_b file_name =
  let lines = read_lines file_name in
  let values = List.map dial lines in
  ticks_b 0 50 values
