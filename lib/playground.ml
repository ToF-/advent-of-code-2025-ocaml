open Utils
open Str

type box = int * int * int

module Circuit = Set.Make (struct
  type t = box

  let compare = compare
end)

module CircuitMap = Map.Make (struct
  type t = box

  let compare = compare
end)

let connected map a b =
  let circuit = map |> CircuitMap.find b in
  circuit |> Circuit.mem a

let connect map a b =
  let circuit_a = map |> CircuitMap.find a in
  let circuit_b = map |> CircuitMap.find b in
  let new_circuit = circuit_a |> Circuit.union circuit_b in
  Circuit.fold
    (fun box acc ->
      acc
      |> CircuitMap.update box (fun opt ->
          match opt with None -> None | Some circuit -> Some new_circuit))
    new_circuit map

let init_map boxes =
  List.fold_left
    (fun map box ->
      map |> CircuitMap.add box (Circuit.empty |> Circuit.add box))
    CircuitMap.empty boxes

let print_box (a, b, c) = Printf.printf "(%d,%d,%d);" a b c

let print_circuit circuit =
  circuit |> Circuit.to_list |> List.iter (fun box -> print_box box)

let print_circuits circuits =
  List.iter
    (fun c ->
      print_circuit c;
      Printf.printf "\n")
    circuits

let print_map map =
  CircuitMap.iter
    (fun box circuit ->
      if Circuit.cardinal circuit > 1 then (
        print_box box;
        Printf.printf "\n[";
        print_circuit circuit;
        Printf.printf "]\n")
      else Printf.printf "\n")
    map

let print_product p =
  List.iter
    (fun (d, (px, py, pz), (qx, qy, qz)) ->
      Printf.printf "(%f, (%d,%d,%d), (%d,%d,%d))\n" d px py pz qx qy qz)
    p

let distance (p1, p2, p3) (q1, q2, q3) =
  let px, py, pz = (float_of_int p1, float_of_int p2, float_of_int p3) in
  let qx, qy, qz = (float_of_int q1, float_of_int q2, float_of_int q3) in
  let dx, dy, dz = (px -. qx, py -. qy, pz -. qz) in
  sqrt ((dx *. dx) +. (dy *. dy) +. (dz *. dz))

let rec all_product f list =
  match list with
  | [] -> []
  | [ a ] -> []
  | x :: rest ->
      List.append (List.map (fun y -> f x y) rest) (all_product f rest)

let box_distance a b = (distance a b, a, b)

let largest_circuit_size map =
  let circuits =
    map |> CircuitMap.bindings |> List.map (fun (k, c) -> Circuit.cardinal c)
  in
  List.fold_left max 0 circuits

let all_connected map =
  let nb_circuits = map |> CircuitMap.bindings |> List.length in
  largest_circuit_size map == nb_circuits

let connect_boxes map distances =
  List.fold_left (fun acc (d, a, b) ->
      let map, pair_opt = acc in
      match pair_opt with
      | Some (a, b) -> (map, pair_opt)
      | None ->
          let new_map = connect map a b in
          if all_connected new_map then (new_map, Some (a, b))
          else (new_map, None))
  (map, None) distances

let rec uniq circuits =
  match circuits with
  | [] -> []
  | [ a ] -> [ a ]
  | a :: b :: rest ->
      let cmp = compare (Circuit.to_list a) (Circuit.to_list b) in
      if cmp == 0 then uniq (b :: rest) else a :: uniq (b :: rest)

let circuits boxes limit =
  let map = init_map boxes in
  let distances = List.sort compare (all_product box_distance boxes) in
  let final_map, pair_opt = connect_boxes map (List.take limit distances) in
  let all_circuits =
    final_map |> CircuitMap.bindings
    |> List.map (fun (k, v) -> v)
    |> List.sort (fun c d -> compare (Circuit.cardinal d) (Circuit.cardinal c))
    |> List.sort (fun c d -> compare (Circuit.to_list c) (Circuit.to_list d))
    |> uniq
    |> List.sort (fun c d -> compare (Circuit.cardinal d) (Circuit.cardinal c))
  in
  (all_circuits, pair_opt)

let circuit_product file_name limit option_b =
  let lines = Utils.read_lines file_name in
  let boxes =
    List.map
      (fun line ->
        match List.map int_of_string (Str.split (Str.regexp ",") line) with
        | [ p; q; r ] -> (p, q, r)
        | _ -> invalid_arg "illegal box value")
      lines
  in
  let all_circuits, pair_opt = circuits boxes limit in
  if option_b then
    match pair_opt with
    | None -> invalid_arg "couldn't connect all boxes"
    | Some ((px, py, pz), (qx, qy, qz)) -> px * qx
  else
    match all_circuits with
    | a :: b :: c :: _ ->
        Circuit.cardinal a * Circuit.cardinal b * Circuit.cardinal c
    | _ -> invalid_arg "not enough circuits"
