open Utils

let acquire lines =
    let (a, b) = List.partition (fun s -> String.contains s '-') lines in
    (List.map (fun s ->
        let (i,jparse_interval a, List.map Int.of_string b)

let fresh_ingredients file_name =
    let lines = Utils.read_lines file_name in
    let (intervals, ingredients) = acquire lines in
    List.length intervals
