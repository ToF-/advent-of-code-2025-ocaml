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
      (fun acc (x, y) -> acc + if IntPairSet.mem (i + x, j + y) all_coords then 1 else 0)
      0 adjacent_coords
  in
  List.fold_left
    (fun acc (i, j) ->
        if nb_adjacent (i,j) < 4 then acc + 1 else acc) 
    0 (IntPairSet.to_list all_coords)

let positions lines =
  List.mapi
    (fun i line ->
      String.to_seqi line
      |> Seq.filter_map (fun (j, c) -> if c = '@' then Some (i, j) else None)
      |> List.of_seq)
    lines
  |> List.concat |> IntPairSet.of_list

let accessible_rolls file_name =
  let lines = Utils.read_lines file_name in
  let coords = positions lines in
  fewer_than_4 coords
