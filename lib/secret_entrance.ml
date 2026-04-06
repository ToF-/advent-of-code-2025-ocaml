let read_lines file_name =
    let lines = String.split_on_char '\n' (In_channel.with_open_bin file_name In_channel.input_all)
    in List.filter (fun s -> s <> "") lines

let dial instruction =
    if String.length instruction < 2 then
        invalid_arg "instruction too short"
    else
        let value = int_of_string (String.sub instruction 1 (String.length instruction - 1)) in
        let sign  = String.sub instruction 0 1 in
        match sign with
        | "L" -> (- value)
        | _ -> value

let rec zeroes counter accum values =
    match values with
   | [] -> counter
   | (x :: xs) -> let new_accum = accum + x in match (new_accum mod 100) == 0 with
   | true -> zeroes (counter + 1) new_accum xs
   | false -> zeroes counter new_accum xs

let actual_password file_name = 
    let lines = (read_lines file_name) in
    let values = List.map dial lines in
    zeroes 0 50 values

