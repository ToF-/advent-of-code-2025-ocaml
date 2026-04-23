open Utils

type machine = { diagram : int; buttons : int list list; joltage : int list }

let read_diagram s =
  let chars = s |> String.to_seq |> List.of_seq |> List.rev in
  snd
    (List.fold_left
       (fun (value, power2) ch ->
         match ch with
         | '#' -> (value lor power2, power2 * 2)
         | _ -> (value, power2 * 2))
       (0, 1) chars)

let read_buttons s =
  String.sub s 1 (String.length s - 2)
  |> String.split_on_char ',' |> List.map int_of_string

let read_joltage = read_buttons

let read_machine line =
  let words = String.split_on_char ' ' line in
  let diagram = read_diagram (List.hd words) in
  let buttons =
    List.map
      (fun s -> read_buttons s)
      (List.drop 1 (List.take (List.length words - 1) words))
  in
  let joltage = read_joltage (List.hd (List.rev words)) in
  { diagram; buttons; joltage }

let switch_light state light = state lxor (1 lsl light)
let switch_lights state lights = List.fold_left switch_light state lights

let rec delete a = function
  | [] -> []
  | x :: xs when x == a -> xs
  | x :: xs -> List.cons x (delete a xs)

let rec permutations = function
  | [] -> [ [] ]
  | xs ->
      List.flatten
        (List.map
           (fun x -> List.map (List.cons x) (permutations (delete x xs)))
           xs)

let rec sublists = function
  | [] -> [ [] ]
  | x :: xs ->
      let rest = sublists xs in
      rest @ List.map (fun l -> x :: l) rest

let subpermutations l = List.flatten (List.map permutations (sublists l))

let print_int_list l = 
    Printf.printf("[");
    List.iter (fun x -> Printf.printf "%d;" x) l ;
    Printf.printf("]\n")


let rec press_combo count state target_state combo =
    match combo with
    | [] -> None
    | lights::rest ->
            Printf.printf "target:%d state:%d lights:" target_state state ;
            print_int_list lights ;
            let new_state = switch_lights state lights in
            Printf.printf "target:%d new state:%d\n***\n" target_state new_state ;
            if new_state == target_state then (Some count) else press_combo (count + 1) new_state target_state rest

let press_combos target_state combos =
    List.fold_left (fun acc combo ->
        let count = press_combo 0 0 target_state combo in
        match count with
        | None -> acc
        | Some n -> min acc n)
    Int.max_int combos

let button_presses file_name option_b =
 let lines = Utils.read_lines file_name in
 let machines = List.map read_machine lines in
 List.fold_left (fun acc m ->
     let combos = subpermutations (m.buttons) in
     let count = press_combos m.diagram combos in
     acc + count)
 0 (List.take 1 machines)

