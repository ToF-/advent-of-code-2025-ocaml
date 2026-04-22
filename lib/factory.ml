open Utils

type machine = {
    diagram : int;
    buttons : int list list;
    joltage : int list; }

let read_diagram s =
    let chars = s |> String.to_seq |> List.of_seq |> List.rev in
    snd (List.fold_left (fun (value, power2) ch ->
        match ch with
        | '#' -> (value lor power2, power2 * 2)
        | _ -> (value, power2 * 2))
    (0, 1) chars)

let read_button s = 
    (String.sub s 1 (String.length s - 2))
    |> String.split_on_char ','
    |> List.map int_of_string

let read_joltage = read_button

let read_machine line =
    let words = String.split_on_char ' ' line in
    let diagram = read_diagram (List.hd words) in
    let buttons = List.map (fun s ->
        read_button s) (List.drop 1 (List.take ((List.length words) -1) words)) in
    let joltage = read_joltage (List.hd (List.rev words)) in
    { diagram = diagram;
      buttons = buttons;
      joltage = joltage; }

let switch_light diagram light = diagram lxor (1 lsl light)

let switch_lights diagram lights =
    List.fold_left switch_light diagram lights


let button_presses file_name option_b =
    let lines = Utils.read_lines file_name in
    let machines = List.map read_machine lines in
    List.length machines
