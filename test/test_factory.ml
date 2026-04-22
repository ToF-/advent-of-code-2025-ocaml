open OUnit2

let tests =
  let option_a = false in
  let option_b = true in
  let sample = "../testdata/day10_sample.txt" in
  let input = "../testdata/day10_input.txt" in
  let button_presses = Advent_of_code_2025.Factory.button_presses in
  Printf.printf "day 10: factory\n";
  "factory"
  >::: [
         ( "fewer button presses for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 7
             (button_presses sample option_a) );
       ]
