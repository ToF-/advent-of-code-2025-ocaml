open OUnit2

let tests =
  let sample = "../testdata/day04_sample.txt" in
  let input = "../testdata/day04_input.txt" in
  let option_b = true in
  let option_a = false in
  let accessible_rolls =
    Advent_of_code_2025.Printing_department.accessible_rolls
  in
  Printf.printf "day 04: printing department\n";
  "printing department"
  >::: [
         ( "accessible rolls for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 13
             (accessible_rolls sample option_a) );
         ( "accessible rolls for input (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 1516
             (accessible_rolls input option_a) );
         ( "accessible rolls for sample (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 43
             (accessible_rolls sample option_b) );
         ( "accessible rolls for input (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 9122
             (accessible_rolls input option_b) );
       ]
