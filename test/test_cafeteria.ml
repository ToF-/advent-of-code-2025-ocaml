open OUnit2

let tests =
  let sample = "../testdata/day05_sample.txt" in
  let fresh_ingredients =
    Advent_of_code_2025.Cafeteria.fresh_ingredients
  in
  Printf.printf "day 05: cafeteria\n";
  "cafeteria"
  >::: [
         ( "fresh ingredients for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 13 (fresh_ingredients sample) );
       ]
