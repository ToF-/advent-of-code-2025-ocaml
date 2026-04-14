open OUnit2

let tests =
  let sample = "../testdata/day06_sample.txt" in
  let input = "../testdata/day06_input.txt" in
  let grand_total_a = Advent_of_code_2025.Trash_compactor.grand_total_a in
  let grand_total_b = Advent_of_code_2025.Trash_compactor.grand_total_b in
  Printf.printf "day 06: trash_compactor\n";
  "cafeteria"
  >::: [
         ( "grand total for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 4277556 (grand_total_a sample) );
         ( "grand total for input (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 6503327062445
             (grand_total_a input) );
         ( "grand total for sample (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 3263827 (grand_total_b sample) );
         ( "grand total for input (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 9640641878593
             (grand_total_b input) );
       ]
