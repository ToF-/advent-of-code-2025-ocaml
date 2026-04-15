open OUnit2

let tests =
  let sample = "../testdata/day07_sample.txt" in
  let input = "../testdata/day07_input.txt" in
  let total_splits = Advent_of_code_2025.Laboratories.total_splits in
  let total_paths = Advent_of_code_2025.Laboratories.total_paths in
  Printf.printf "day 07: laboratories\n";
  "laboratories"
  >::: [
         ( "total splits for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 21 (total_splits sample) );
         ( "total splits for input (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 1660 (total_splits input) );
         ( "total paths for sample (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 40 (total_paths sample) );
         ( "total paths for input (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 0 (total_paths input) );
       ]
