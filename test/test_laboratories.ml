open OUnit2

let tests =
  let sample = "../testdata/day07_sample.txt" in
  let input =  "../testdata/day07_input.txt" in
  let total_splits = Advent_of_code_2025.Laboratories.total_splits in
  Printf.printf "day 07: laboratories\n";
  "laboratories"
  >::: [
         ( "total splits for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 21 (total_splits sample) );
         ( "total splits for input (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 1660 (total_splits input) );
       ]
