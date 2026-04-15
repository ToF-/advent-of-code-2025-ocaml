open Utils
module IntSet = Set.Make (Int)

let find_source line =
  match Seq.find_index (fun c -> c == 'S') (String.to_seq line) with
  | Some n -> n
  | None -> invalid_arg "no source found"

let split_beams line beams =
  Seq.fold_lefti
    (fun (splits, beams) pos c ->
      if c == '^' && beams |> IntSet.mem pos then
        ( splits + 1,
          beams |> IntSet.remove pos
          |> IntSet.add (pos - 1)
          |> IntSet.add (pos + 1) )
      else (splits, beams))
    (0, beams) (String.to_seq line)

let print_beams beams =
  let rec print_beams_pos pos =
    match pos with
    | [] -> Printf.printf "\n"
    | pos :: rest ->
        Printf.printf " %d " pos;
        print_beams_pos rest
  in
  print_beams_pos (IntSet.to_list beams)

let count_splits lines initial_beams =
  fst
    (List.fold_left
       (fun acc line ->
         let splits, beams = acc in
         if String.contains line '^' then
           let new_splits, new_beams = split_beams line beams in
           (splits + new_splits, new_beams)
         else (splits, beams))
       (0, initial_beams) lines)

let total_splits file_name =
  match Utils.read_lines file_name with
  | [] -> invalid_arg "empty input file"
  | head :: lines ->
      let beams = IntSet.empty |> IntSet.add (find_source head) in
      count_splits lines beams

let splitter_set line =
  Seq.fold_lefti
    (fun acc i c -> if c == '^' then acc |> IntSet.add i else acc)
    IntSet.empty (String.to_seq line)

let nb_paths initial_pos lines =
  List.length
    (List.fold_left
       (fun acc_positions line ->
           Printf.printf "%d\n" (List.length acc_positions) ;
         if String.contains line '^' then
           let splitters = splitter_set line in
           List.fold_left
             (fun positions position ->
               if splitters |> IntSet.mem position then
                 (position - 1) :: (position + 1) :: positions
               else position :: positions)
             [] acc_positions
         else acc_positions)
       [ initial_pos ] lines)

let total_paths file_name =
  match Utils.read_lines file_name with
  | [] -> invalid_arg "empty input file"
  | head :: lines ->
      let initial_pos = find_source head in
      nb_paths initial_pos (List.take 90 lines)
