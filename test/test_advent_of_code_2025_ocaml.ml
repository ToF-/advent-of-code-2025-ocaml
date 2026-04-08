open OUnit2

let secret_entrance =
  let password = Advent_of_code_2025.Secret_entrance.password in
  let stops_at_zero = Advent_of_code_2025.Secret_entrance.stops_at_zero in
  let passes_on_zero = Advent_of_code_2025.Secret_entrance.passes_on_zero in
  let sample = "../testdata/day01_sample.txt" in
  let input = "../testdata/day01_input.txt" in
  Printf.printf "day 01: secret entrance\n";
  "secret entrance"
  >::: [
         ( "secret entrance a with sample" >:: fun _ ->
           assert_equal ~printer:string_of_int 3 (password sample stops_at_zero)
         );
         ( "secret entrance a with input" >:: fun _ ->
           assert_equal ~printer:string_of_int 964
             (password input stops_at_zero) );
         ( "secret entrance b with sample" >:: fun _ ->
           assert_equal ~printer:string_of_int 6
             (password sample passes_on_zero) );
         ( "secret entrance b with input" >:: fun _ ->
           assert_equal ~printer:string_of_int 5872
             (password input passes_on_zero) );
       ]

let gift_shop =
  let sum_invalid_ids = Advent_of_code_2025.Gift_shop.sum_invalid_ids in
  let sum_invalid_ids_from_file =
    Advent_of_code_2025.Gift_shop.sum_invalid_ids_from_file
  in
  let sample = "../testdata/day02_sample.txt" in
  let input  = "../testdata/day02_input.txt" in
  Printf.printf "day 02: gift shop\n";
  "gift shop"
  >::: [
         ( "sum of invalid IDs example 11 to 99" >:: fun _ ->
           assert_equal ~printer:string_of_int
             (11 + 22 + 33 + 44 + 55 + 66 + 77 + 88 + 99)
             (sum_invalid_ids [ (11, 99) ]) );
         ( "sum of invalid IDs example 44 to 66" >:: fun _ ->
           assert_equal ~printer:string_of_int
             (44 + 55 + 66)
             (sum_invalid_ids [ (44, 66) ]) );
         ( "sum of invalid IDs example 21 to 57" >:: fun _ ->
           assert_equal ~printer:string_of_int
             (22 + 33 + 44 + 55)
             (sum_invalid_ids [ (21, 57) ]) );
         ( "sum of invalid IDs example 17 to 52" >:: fun _ ->
           assert_equal ~printer:string_of_int
             (22 + 33 + 44)
             (sum_invalid_ids [ (17, 52) ]) );
         ( "sum of invalid IDs example 1000 to 2000" >:: fun _ ->
           assert_equal ~printer:string_of_int
             (1010 + 1111 + 1212 + 1313 + 1414 + 1515 + 1616 + 1717 + 1818
            + 1919)
             (sum_invalid_ids [ (1000, 2000) ]) );
         ( "sum of invalid IDs example 100000 to 101000" >:: fun _ ->
           assert_equal ~printer:string_of_int
             (100100 + 101101 + 102102 + 103103)
             (sum_invalid_ids [ (100000, 104000) ]) );
         ( "sum of invalid IDs example 95 to 115" >:: fun _ ->
           assert_equal ~printer:string_of_int 99
             (sum_invalid_ids [ (95, 115) ]) );
         ( "sum of invalid IDs example 998 to 1012" >:: fun _ ->
           assert_equal ~printer:string_of_int 1010
             (sum_invalid_ids [ (998, 1012) ]) );
         ( "sum of invalid IDs for sample" >:: fun _ ->
           assert_equal ~printer:string_of_int
           1227775554 (sum_invalid_ids_from_file sample) );
         ( "sum of invalid IDs for input" >:: fun _ ->
           assert_equal ~printer:string_of_int
           1227775554 (sum_invalid_ids_from_file input) );
       ]

let _ = List.map run_test_tt_main [ secret_entrance; gift_shop ]
