open Utils

let acquire lines =
    let parse_interval s =
        match String.split_on_char '-' s with
        | [s1;s2] -> (int_of_string s1, int_of_string s2)
        | _ -> invalid_arg "incorrect interval"
    in
    let (ss1, ss2) = List.partition (fun s -> String.contains s '-') lines in
    (List.map (fun s -> parse_interval s) ss1,
    List.map (fun s -> int_of_string s) ss2)

let fresh_ingredients file_name =
    let lines = Utils.read_lines file_name in
    let (intervals, ingredients) = acquire lines in
    List.length intervals
