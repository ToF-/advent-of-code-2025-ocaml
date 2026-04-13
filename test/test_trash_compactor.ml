open OUnit2

let tests =
  let sample = "../testdata/day06_sample.txt" in
  let grand_total = Advent_of_code_2025.Trash_compactor.grand_total in
  Printf.printf "day 06: trash_compactor\n";
  "cafeteria"
  >::: [
         ( "fresh ingredients for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 0
             (grand_total sample) );
       ]
