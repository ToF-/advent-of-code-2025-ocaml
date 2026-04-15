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
  map
  |> CircuitMap.update b (fun opt ->
      match opt with None -> None | Some circuit -> Some new_circuit)
  |> CircuitMap.update a (fun opt ->
      match opt with None -> None | Some circuit -> Some new_circuit)

let init_map boxes =
  List.fold_left
    (fun map box ->
      map |> CircuitMap.add box (Circuit.empty |> Circuit.add box))
    CircuitMap.empty boxes

let print_box (a,b,c) = 
    Printf.printf "(%d,%d,%d);" a b c

let print_circuit circuit = 
    circuit |> Circuit.to_list |> List.iter (fun box ->
        print_box box)


let print_map map = 
    CircuitMap.iter (fun box circuit ->
        print_box box ;
        Printf.printf "\n[" ;
        print_circuit circuit ;
        Printf.printf "]\n") map

let circuit_product file_name =
  let lines = Utils.read_lines file_name in
  let boxes =
    List.map
      (fun line ->
        match List.map int_of_string (Str.split (Str.regexp ",") line) with
        | [ p; q; r ] -> (p, q, r)
        | _ -> invalid_arg "illegal box value")
      lines
  in
  let map = init_map boxes in
  print_map map ;
  List.length boxes
