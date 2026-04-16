open OUnit2

let tests =
  let option_a = false in
  let option_b = true in
  let sample = "../testdata/day08_sample.txt" in
  let input = "../testdata/day08_input.txt" in
  let circuit_product = Advent_of_code_2025.Playground.circuit_product in
  Printf.printf "day 08: playground\n";
  "playground"
  >::: [
         ( "product for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 40
             (circuit_product sample 10 option_a) );
         ( "product for input (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 79056
             (circuit_product input 1000 option_a) );
         ( "product for sample (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 25272
             (circuit_product sample 400 option_b) );
         ( "product for input (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 4639477
             (circuit_product input 1000000 option_b) );
       ]
