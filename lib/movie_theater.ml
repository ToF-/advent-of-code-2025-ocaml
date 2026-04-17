open Utils


let rec largest_product coords =
    match coords with
    | [] -> 0
    | [_] -> 0
    | ((px,py)::rest) -> 
            let a = List.fold_left (fun acc (qx,qy) ->
                max acc ((abs (qx - px) + 1)  * (abs (qy - py) + 1))
            ) 0 rest in
            max a (largest_product rest)

let largest_area file_name = 
    let lines = Utils.read_lines file_name in
    let coords = List.map (fun s ->
        match String.split_on_char ',' s with
        | [a;b] -> (int_of_string a, int_of_string b)
        | _ -> invalid_arg "illegal coord")
    lines
    in
    largest_product coords 
