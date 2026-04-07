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

let modulo x y =
  let r = x mod y in
  if r >= 0 then r else r + y

let rec stops_at_zero counter position values =
  match values with
  | [] -> counter
  | delta :: rest ->
      let new_position = position + delta in
      if new_position mod 100 == 0 then
        stops_at_zero (counter + 1) new_position rest
      else stops_at_zero counter new_position rest

let rec passes_on_zero counter position values =
  match values with
  | [] -> counter
  | value :: rest ->
      let new_position = position + value in
      let passes =
        (abs new_position / 100)
        +
        match new_position with
        | 0 -> 1
        | p when p < 0 && position <> 0 -> 1
        | _ -> 0
      in
      passes_on_zero (counter + passes) (modulo new_position 100) rest

let password file_name f =
  let lines = read_lines file_name in
  let values = List.map dial lines in
  f 0 50 values
