open Utils

type machine = {
    diagram : int;
    buttons : int list list;
    joltage : int list; }

let read_button s = 
    s |> String.sub 1 (String.length s - 2)
    |> String.split_on_char ','
    |> List.map int_of_string

let read_machine line =
    let words = String.split_on_char ' ' in
    let diagram = read_diagram (List.hd words) in
    let buttons = List.map (fun s ->
        read_button s) (List.drop 1 (List.take ((List.length words) -1) words)) in
    let joltage = read_joltage (List.hd (List.rev words)) in
    { diagram = diagram;
      buttons = buttons;
      joltage = joltage; }
