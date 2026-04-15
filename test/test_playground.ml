open OUnit2

let tests =
  let sample = "../testdata/day08_sample.txt" in
  let input =  "../testdata/day08_input.txt" in
  let circuit_product = Advent_of_code_2025.Playground.circuit_product in
  Printf.printf "day 07: laboratories\n";
  "laboratories"
  >::: [
         ( "total splits for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 40 (circuit_product sample) );
         ( "total splits for input (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 0 (circuit_product input) );
       ]
