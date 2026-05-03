open Utils

module SwitchQueue = Pqueue.MakeMin (struct
  type t = int * int * int

  let compare (p1, _, _) (p2, _, _) = compare p1 p2
end)

module IntSet = Set.Make (Int)

let print_queue queue =
  let to_print = SwitchQueue.copy queue in
  let rec print_queue_aux q =
    match SwitchQueue.pop_min q with
    | Some (count, state, switches) ->
        Printf.printf "(%d--%d--%d) " count state switches
    | None -> Printf.printf "\n"
  in
  print_queue_aux to_print

let rec search_combo combos visited queue =
  (* print_queue queue; *)
  match SwitchQueue.pop_min queue with
  | None -> None
  | Some (count, state, switches) ->
      let new_state = state lxor switches in
      if new_state == 0 then Some count
      else if visited |> IntSet.mem new_state then
        search_combo combos visited queue
      else (
        List.iter
          (fun switch_set ->
            SwitchQueue.add queue (count + 1, new_state, switch_set))
          combos;
        let new_visited = visited |> IntSet.add new_state in
        search_combo combos new_visited queue)

type machine = { diagram : int; buttons : int list; joltage : int }

let min_presses m =
  let combos = m.buttons in
  let state = m.diagram in
  let queue =
    SwitchQueue.of_list (List.map (fun switches -> (1, state, switches)) combos)
  in
  let visited = IntSet.empty in
  match search_combo combos visited queue with Some count -> count | None -> 0

let read_diagram s =
  let chars = s |> String.to_seq |> List.of_seq in
  fst
    (List.fold_left
       (fun (value, power2) ch ->
         match ch with
         | '#' -> (value lor power2, power2 * 2)
         | '.' -> (value, power2 * 2)
         | _ -> (value, power2))
       (0, 1) chars)

let read_button s =
  let lights =
    String.sub s 1 (String.length s - 2)
    |> String.split_on_char ',' |> List.map int_of_string
  in
  List.fold_left (fun acc light -> acc lor (1 lsl light)) 0 lights

let read_joltage = read_button

let read_machine line =
  let words = String.split_on_char ' ' line in
  let diagram = read_diagram (List.hd words) in
  let buttons =
    List.map
      (fun s -> read_button s)
      (List.drop 1 (List.take (List.length words - 1) words))
  in
  let joltage = read_joltage (List.hd (List.rev words)) in
  { diagram; buttons; joltage }

let button_presses file_name option_b =
  let lines = Utils.read_lines file_name in
  let machines = List.map read_machine lines in
  List.fold_left (fun acc m -> acc + min_presses m) 0 machines

let button_switches button =
  let rec add_switch button i =
    match button with
    | 0 -> []
    | n when n land 1 == 1 -> List.cons i (add_switch (button lsr 1) (i + 1))
    | _ -> add_switch (button lsr 1) (i + 1)
  in
  add_switch button 0

let light_joltages joltages = button_switches

let list_to_matrix list =
  let nb_rows = list |> List.length in
  let nb_cols = list |> List.hd |> List.length in
  let matrix = Array.make_matrix nb_rows nb_cols 0 in
  let _ =
    list
    |> List.iteri (fun i row ->
        row |> List.iteri (fun j x -> matrix.(i).(j) <- x))
  in
  matrix

let matrix_to_list matrix =
  matrix |> Array.to_list |> List.map (fun a -> a |> Array.to_list)

let reduce matrix =
  let nb_rows = matrix |> Array.length in
  let nb_cols = matrix.(0) |> Array.length in
  let print_matrix () =
    Printf.printf "matrix\n";
    for row = 0 to nb_rows - 1 do
      for col = 0 to nb_cols - 1 do
        Printf.printf "%d " matrix.(row).(col)
      done;
      Printf.printf "\n"
    done
  in
  let rec pivot col row =
    if row < nb_rows then
      if matrix.(row).(col) == 0 then pivot col (row + 1) else Some row
    else None
  in

  let swap_rows a b =
    Printf.printf "swap\n";
    for col = 0 to nb_cols - 1 do
      let tmp = matrix.(b).(col) in
      matrix.(b).(col) <- matrix.(a).(col);
      matrix.(a).(col) <- tmp
    done
  in
  let revert_row row =
    for col = 0 to nb_cols - 1 do
      matrix.(row).(col) <- -matrix.(row).(col)
    done
  in
  let subtract a b =
    for col = 0 to nb_cols - 1 do
      matrix.(a).(col) <- matrix.(a).(col) - matrix.(b).(col)
    done
  in
  if nb_rows > 1 then
    for d = 0 to nb_rows - 2 do
      print_matrix ();
      Printf.printf "%d\n" d;
      if matrix.(d).(d) == 0 then
        match pivot d d with None -> () | Some p -> swap_rows d p
      else if matrix.(d).(d) < 0 then revert_row d;
      for row = d + 1 to nb_rows - 1 do
        if matrix.(row).(d) < 0 then revert_row row;
        if matrix.(row).(d) > 0 then subtract row d
      done;
      Printf.printf "done with col %d\n" d
    done;
    matrix

