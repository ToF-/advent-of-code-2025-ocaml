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

let rec ticks_a counter accum values =
    match values with
   | [] -> counter
   | (x :: xs) -> let new_accum = accum + x in match (new_accum mod 100) == 0 with
   | true -> ticks_a (counter + 1) new_accum xs
   | false -> ticks_a counter new_accum xs

let actual_password_a file_name = 
    let lines = (read_lines file_name) in
    let values = List.map dial lines in
    ticks_a 0 50 values

let modulo x y =
    let r = x mod y in
    if r >= 0 then r else r + y

let left_clicks pos delta =
    if delta >= pos
    then 1 + (delta - pos) / 100
    else 0

let right_clicks pos delta = 
    if pos + delta >= 100
    then 1 + (pos + delta - 100) / 100
    else 0


let rec ticks_b counter accum values = 
    Printf.printf "%d %d\n" counter accum ;
    match values with
    | [] -> counter
    | (x :: xs) -> let new_accum = modulo (accum + x) 100 in
    if x < 0 then
        ticks_b (counter + (left_clicks accum (abs x))) new_accum xs
    else
        ticks_b (counter + (right_clicks accum x)) new_accum xs

let actual_password_b file_name = 
    let lines = (read_lines file_name) in
    let values = List.map dial lines in
    ticks_b 0 50 values
