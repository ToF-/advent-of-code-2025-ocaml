
open Utils
open Str

let acquire file_name =
    let lines = Utils.read_lines file_name in
    let nb_rows = (List.length lines) - 1 in
    let acquire_line count lines =
        if count < nb_rows then
            let (line::rest) = lines in
            let numbers = List.map int_of_string (Str.split (Str.regexp "[ ]+") line)

    (Array.init_matrix nb_rows 10 (fun i j -> 0), Array.init 10 (fun _ -> ( + )))

let grand_total file_name = 0
