open Utils

let area (px, py) (qx, qy) = (abs (qx - px) + 1) * (abs (qy - py) + 1)

let rec largest_product coords =
  match coords with
  | [] -> 0
  | [ _ ] -> 0
  | (px, py) :: rest ->
      let a =
        List.fold_left
          (fun acc (qx, qy) -> max acc (area (px, py) (qx, qy)))
          0 rest
      in
      max a (largest_product rest)

let sorted_edge edge =
  let (px, py), (qx, qy) = edge in
  let reversed = ((qx, qy), (px, py)) in
  match compare px qx with
  | -1 -> edge
  | 1 -> reversed
  | _ -> ( match compare py qy with -1 -> edge | 1 -> reversed | _ -> edge)

let largest_green_area coords =
  let n = List.length coords in
  let last_coord = List.hd (List.rev coords) in
  let next_coords = List.cons last_coord (List.take (n - 1) coords) in
  let edges =
    List.map (fun edge -> sorted_edge edge) (List.combine coords next_coords)
  in
  let areas =
    fst
      (List.fold_left
         (fun (acc, i) c1 ->
           let new_acc =
             List.map
               (fun c2 ->
                 let n1, n2 = sorted_edge (c1, c2) in
                 (area n1 n2, n1, n2))
               (List.drop i coords)
           in
           (List.append acc new_acc, i + 1))
         ([], 0) coords)
  in
  let sorted_areas =
    areas |> List.sort (fun (d1, p1, q1) (d2, p2, q2) -> compare d2 d1)
  in
  let rec largest areas =
    match areas with
    | [] -> 0
    | area :: rest ->
        let dist, (px, py), (qx, qy) = area in
        let py, qy = if py <= qy then (py, qy) else (qy, py) in
        if
          not
            (List.exists
               (fun ((rx, ry), (sx, sy)) ->
                 sx > px && rx < qx && sy > py && ry < qy)
               edges)
        then dist
        else largest rest
  in
  largest sorted_areas

let largest_area file_name option_b =
  let lines = Utils.read_lines file_name in
  let coords =
    List.map
      (fun s ->
        match String.split_on_char ',' s with
        | [ a; b ] -> (int_of_string a, int_of_string b)
        | _ -> invalid_arg "illegal coord")
      lines
  in
  if option_b then largest_green_area coords else largest_product coords
