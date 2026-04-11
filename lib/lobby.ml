let ints line =
  Array.of_seq
    (Seq.map (fun c -> Char.code c - Char.code '0') (String.to_seq line))

let rec pow_10 i = match i with 0 -> 1 | _ -> 10 * pow_10 (i - 1)

let joltage_matrix size batteries =
  let length = Array.length batteries in
  let initial = Array.init_matrix size length (fun _ _ -> 0) in
  let is = List.init size (fun i -> i) in
  let js = List.init length (fun j -> length - j - 1) in

  let set_joltage_matrix (i, j) matrix =
    match i with
    | 0 ->
        let _ =
          matrix.(0).(j) <-
            (if j < length - 1 then max batteries.(j) matrix.(0).(j + 1)
             else batteries.(j))
        in
        matrix
    | _ ->
        if (j + i) < length
        then
          let _ =
            matrix.(i).(j) <-
              max
                ((batteries.(j) * pow_10 i) + matrix.(i - 1).(j + 1))
                matrix.(i).(j + 1)
          in
          matrix
        else
            matrix
  in

  let fold_joltage i initial =
    List.fold_left (fun matrix j -> set_joltage_matrix (i, j) matrix) initial js
  in
  let result =
    List.fold_left (fun matrix i -> fold_joltage i matrix) initial is
  in
  result

let max_joltage size batteries =
  let matrix = joltage_matrix size batteries in
  matrix.(size - 1).(0)

let total_joltage file_name size =
  let lines = Utils.read_lines file_name in
  List.fold_left ( + ) 0
    (List.map (fun line -> max_joltage size (ints line)) lines)
