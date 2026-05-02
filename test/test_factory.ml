open OUnit2

let tests =
  let option_a = false in
  let sample = "../testdata/day10_sample.txt" in
  let input = "../testdata/day10_input.txt" in
  let button_presses = Advent_of_code_2025.Factory.button_presses in
  let button_switches = Advent_of_code_2025.Factory.button_switches in
  Printf.printf "day 10: factory\n";
  "factory"
  >::: [
         ( "fewer button presses for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 7
             (button_presses sample option_a) );
         ( "fewer button presses for input (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 512
             (button_presses input option_a) );
         ( "switches command of a button" >:: fun _ ->
           assert_equal [ 0; 4; 5 ] (button_switches (1 + 16 + 32)) );
       ]
