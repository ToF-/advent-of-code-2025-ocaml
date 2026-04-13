module IntPair = struct
  type t = int * int

  let compare = compare
end

module IntPairSet = Set.Make (IntPair)
module IntPairMap = Map.Make (IntPair)

let fewer_than_4 all_coords =
  let adjacent_coords =
    [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]
  in
  let nb_adjacent (i, j) =
    List.fold_left
      (fun acc (x, y) ->
        acc + if IntPairSet.mem (i + x, j + y) all_coords then 1 else 0)
      0 adjacent_coords
  in
  List.fold_left
    (fun acc (i, j) ->
      if nb_adjacent (i, j) < 4 then List.cons (i, j) acc else acc)
    []
    (IntPairSet.to_list all_coords)

let positions lines =
  List.mapi
    (fun i line ->
      String.to_seqi line
      |> Seq.filter_map (fun (j, c) -> if c = '@' then Some (i, j) else None)
      |> List.of_seq)
    lines
  |> List.concat |> IntPairSet.of_list

let remove_coords coords set =
  List.fold_left (fun acc (i, j) -> acc |> IntPairSet.remove (i, j)) set coords

let removable all_coords =
  let rec remove_rolls set =
    let rolls_to_remove = fewer_than_4 set in
    match rolls_to_remove with
    | [] -> set
    | _ ->
        let new_set = remove_coords rolls_to_remove set in
        remove_rolls new_set
  in
  let initial_count = all_coords |> IntPairSet.cardinal in
  initial_count - (remove_rolls all_coords |> IntPairSet.cardinal)

let accessible_rolls file_name option_b =
  let lines = Utils.read_lines file_name in
  let coords = positions lines in
  if option_b then removable coords else List.length (fewer_than_4 coords)
