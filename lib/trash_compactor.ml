open Utils
open Str

let words line = Str.split (Str.regexp "[ ]+") line

let acquire_operations line operations initial_values =
  List.iteri
    (fun i op ->
      if String.equal "*" op then
        let _ = operations.(i) <- ( * ) in
        initial_values.(i) <- 1
      else
        let _ = operations.(i) <- ( + ) in
        initial_values.(i) <- 0)
    (words line)

let acquire_a file_name =
  let lines = Utils.read_lines file_name in
  let nb_rows = List.length lines - 1 in
  let nb_cols = List.length (words (List.hd lines)) in
  let numbers = Array.init_matrix nb_rows nb_cols (fun _ _ -> 0) in
  let operations = Array.init nb_cols (fun _ -> ( + )) in
  let initial_values = Array.init nb_cols (fun _ -> 0) in
  let rec acquire_lines row lines =
    match lines with
    | [] -> ()
    | [ line ] -> acquire_operations line operations initial_values
    | line :: rest ->
        List.iteri
          (fun i s -> numbers.(row).(i) <- int_of_string s)
          (words line);
        acquire_lines (row + 1) rest
  in
  acquire_lines 0 lines;
  (nb_rows, nb_cols, numbers, operations, initial_values)

let grand_total_a file_name =
  let nb_rows, nb_cols, numbers, operations, initial_values =
    acquire_a file_name
  in
  let cols = List.init nb_cols (fun i -> i) in
  let rows = List.init nb_rows (fun i -> i) in
  List.fold_left
    (fun acc_col col ->
      let op = operations.(col) in
      let fold =
        List.fold_left
          (fun acc_row row -> op acc_row numbers.(row).(col))
          initial_values.(col) rows
      in
      acc_col + fold)
    0 cols

let acquire_b file_name =
  let lines = Utils.read_lines file_name in
  let nb_cols = List.length (words (List.hd lines)) in
  let operations = Array.init nb_cols (fun _ -> ( + )) in
  let initial_values = Array.init nb_cols (fun _ -> 0) in
  let rec acquire_lines lines =
    match lines with
    | [] -> []
    | [ line ] ->
        acquire_operations line operations initial_values;
        []
    | line :: rest -> line :: acquire_lines rest
  in
  let seq = List.to_seq (List.map String.to_seq (acquire_lines lines)) in
  let all_numbers =
    List.of_seq
      (Seq.map
         (fun sq ->
           let st = String.trim (String.of_seq sq) in
           let n = if String.equal "" st then 0 else int_of_string st in
           n)
         (Seq.transpose seq))
  in
  (all_numbers, operations, initial_values)

type t = int list [@@deriving show]

let grand_total_b file_name =
  let numbers, operations, initial_values = acquire_b file_name in
  let rec totalize total acc count numbers =
    match numbers with
    | [] ->
        total + acc
    | 0 :: rest ->
        let new_count = count + 1 in
        let initial_value = initial_values.(new_count) in
        totalize (total + acc) initial_value new_count rest
    | number :: rest ->
        let operation = operations.(count) in
        let new_acc = operation acc number in
        totalize total new_acc count rest
  in
  totalize 0 initial_values.(0) 0 numbers
