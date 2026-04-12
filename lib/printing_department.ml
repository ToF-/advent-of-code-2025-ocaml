
module IntPair = struct
    type t = int * int
    let compare = compare
end

module IntPairSet = Set.Make(IntPair)

module IntPairMap = Map.Make(IntPair)

let set_roll_set lines =
    List.iteri (fun (i line) ->
        let seq = String.to_seq line in
        Seq.iteri (fun j c ->
            coords.(IntPairSet.empty

let accessible_rolls file_name =
    let lines = Utils.read_lines file_name in
    List.length (IntPairSet.to_list (set_roll_set lines))
