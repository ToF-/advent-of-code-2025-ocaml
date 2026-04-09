open OUnit2

let tests =
  let sum_invalid_ids = Advent_of_code_2025.Gift_shop.sum_invalid_ids in
  let sum_invalid_ids_from_file =
    Advent_of_code_2025.Gift_shop.sum_invalid_ids_from_file
  in
  let sum_ids_a = Advent_of_code_2025.Gift_shop.sum_ids_a in
  let sum_ids_b = Advent_of_code_2025.Gift_shop.sum_ids_b in
  let sample = "../testdata/day02_sample.txt" in
  let input = "../testdata/day02_input.txt" in
  Printf.printf "day 02: gift shop\n";
  "gift shop"
  >::: [
         ( "sum of invalid IDs example 11 to 99 (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int
             (11 + 22 + 33 + 44 + 55 + 66 + 77 + 88 + 99)
             (sum_invalid_ids [ (11, 99) ] sum_ids_a) );
         ( "sum of invalid IDs example 44 to 66 (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int
             (44 + 55 + 66)
             (sum_invalid_ids [ (44, 66) ] sum_ids_a) );
         ( "sum of invalid IDs example 21 to 57 (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int
             (22 + 33 + 44 + 55)
             (sum_invalid_ids [ (21, 57) ] sum_ids_a) );
         ( "sum of invalid IDs example 17 to 52 (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int
             (22 + 33 + 44)
             (sum_invalid_ids [ (17, 52) ] sum_ids_a) );
         ( "sum of invalid IDs example 1000 to 2000 (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int
             (1010 + 1111 + 1212 + 1313 + 1414 + 1515 + 1616 + 1717 + 1818
            + 1919)
             (sum_invalid_ids [ (1000, 2000) ] sum_ids_a) );
         ( "sum of invalid IDs example 100000 to 101000 (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int
             (100100 + 101101 + 102102 + 103103)
             (sum_invalid_ids [ (100000, 104000) ] sum_ids_a) );
         ( "sum of invalid IDs example 95 to 115 (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 99
             (sum_invalid_ids [ (95, 115) ] sum_ids_a) );
         ( "sum of invalid IDs example 998 to 1012 (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 1010
             (sum_invalid_ids [ (998, 1012) ] sum_ids_a) );
         ( "sum of invalid IDs for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 1227775554
             (sum_invalid_ids_from_file sample sum_ids_a) );
         ( "sum of invalid IDs for input (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 29940924880
             (sum_invalid_ids_from_file input sum_ids_a) );
         ( "sum of invalid IDs example 95 to 115 (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int (99 + 111)
             (sum_invalid_ids [ (95, 115) ] sum_ids_b) );
         ( "sum of invalid IDs example 1000 to 2000 (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int
             (1010 + 1111 + 1212 + 1313 + 1414 + 1515 + 1616 + 1717 + 1818
            + 1919)
             (sum_invalid_ids [ (1000, 2000) ] sum_ids_b) );
         ( "sum of invalid IDs example 565653 to 565659 (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 565656
             (sum_invalid_ids [ (565653, 565659) ] sum_ids_b) );
         ( "sum of invalid IDs example 2121212118 to 2121212124 (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 2121212121
             (sum_invalid_ids [ (2121212118,2121212124) ] sum_ids_b) );
         ( "sum of invalid IDs example 824824821 to 824824827 (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 824824824
             (sum_invalid_ids [ (824824821, 824824827) ] sum_ids_b) );
         ( "sum of invalid IDs example 38593856 to 38593862 (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 38593859
             (sum_invalid_ids [ (38593856,38593862) ] sum_ids_b) );
         ( "sum of invalid IDs for sample (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 4174379265
             (sum_invalid_ids_from_file sample sum_ids_b) );
         ( "sum of invalid IDs for input (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 48631959042
             (sum_invalid_ids_from_file input sum_ids_b) );
       ]
