let ints line =
  Seq.map (fun c -> Char.code c - Char.code '0') (String.to_seq line)

let rec max_dual batteries =
  let rec max_joltage batteries =
    match Seq.uncons batteries with
    | None -> 0
    | Some (u, rest) ->
        let v = max_joltage rest in
        if u > v then u else v
  in
  match Seq.uncons batteries with
  | None -> 0
  | Some (t, units) -> (
      match Seq.uncons units with
      | None -> t
      | Some (u, rest) ->
          let a = (t * 10) + max_joltage (Seq.cons u rest) in
          let b = max_dual (Seq.cons u rest) in
          if a > b then a else b)

let total_joltage file_name =
  let lines = Utils.read_lines file_name in
  List.fold_left ( + ) 0 (List.map (fun line -> max_dual (ints line)) lines)
