open OUnit2

let tests =
  let sample = "../testdata/day05_sample.txt" in
  let input = "../testdata/day05_input.txt" in
  let option_a = false in
  let option_b = true in
  let fresh_ingredients = Advent_of_code_2025.Cafeteria.fresh_ingredients in
  Printf.printf "day 05: cafeteria\n";
  "cafeteria"
  >::: [
         ( "fresh ingredients for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 3
             (fresh_ingredients sample option_a) );
         ( "fresh ingredients for input (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 707
             (fresh_ingredients input option_a) );
         ( "fresh ingredients for sample (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 14
             (fresh_ingredients sample option_b) );
       ]
