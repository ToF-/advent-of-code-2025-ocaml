open Utils
open Str

let words line = Str.split (Str.regexp "[ ]+") line

let acquire file_name =
  let lines = Utils.read_lines file_name in
  let nb_rows = List.length lines - 1 in
  let nb_cols = List.length (words (List.hd lines)) in
  let numbers = Array.init_matrix nb_rows nb_cols (fun _ _ -> 0) in
  let operations = Array.init nb_cols (fun _ -> ( + )) in
  let initial_values = Array.init nb_cols (fun _ -> 0) in 
  let rec acquire_lines row lines =
    match lines with
    | [] -> ()
    | [ line ] ->
        List.iteri
          (fun i op -> if String.equal "*" op then
              let _ = operations.(i) <- ( * )  in initial_values.(i) <- 1 
          else 
              let _ = operations.(i) <- ( + ) in initial_values.(i) <- 0 )
          (words line)
    | line :: rest ->
        List.iteri
          (fun i s -> numbers.(row).(i) <- int_of_string s)
          (words line);
        acquire_lines (row + 1) rest
  in
  acquire_lines 0 lines;
  (nb_rows, nb_cols, numbers, operations, initial_values)

let grand_total file_name =
  let nb_rows, nb_cols, numbers, operations, initial_values = acquire file_name in
  let cols = List.init nb_cols (fun i -> i) in
  let rows = List.init nb_rows (fun i -> i) in
  List.fold_left 
  (fun acc_col col ->
      let op = operations.(col) in
      let fold = (List.fold_left
      (fun acc_row row ->
          op acc_row numbers.(row).(col)
      )
      initial_values.(col) rows)
      in
      acc_col + fold)
  0 cols
