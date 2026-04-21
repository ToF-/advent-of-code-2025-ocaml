open OUnit2

let tests =
  let option_a = false in
  let option_b = true in
  let sample = "../testdata/day09_sample.txt" in
  let input = "../testdata/day09_input.txt" in
  let largest_area = Advent_of_code_2025.Movie_theater.largest_area in
  Printf.printf "day 09: movie theater\n";
  "movie theater"
  >::: [
         ( "largest area for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 50 (largest_area sample option_a)
         );
         ( "largest area for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 4749672288
             (largest_area input option_a) );
         ( "largest area for sample (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 24 (largest_area sample option_b)
         );
         ( "largest area for input (B)" >:: fun _ ->
           assert_equal ~printer:string_of_int 1479665889 (largest_area input option_b)
         );
       ]
