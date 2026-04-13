open OUnit2

let tests =
  let sample = "../testdata/day05_sample.txt" in
  let input  = "../testdata/day05_input.txt" in
  let fresh_ingredients =
    Advent_of_code_2025.Cafeteria.fresh_ingredients
  in
  Printf.printf "day 05: cafeteria\n";
  "cafeteria"
  >::: [
         ( "fresh ingredients for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 3 (fresh_ingredients sample) );
         ( "fresh ingredients for input (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 707 (fresh_ingredients input) );
       ]
