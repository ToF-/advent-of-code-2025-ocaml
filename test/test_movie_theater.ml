open OUnit2

let tests =
  let sample = "../testdata/day09_sample.txt" in
  let input = "../testdata/day09_input.txt" in
  let largest_area = Advent_of_code_2025.Movie_theater.largest_area in
  Printf.printf "day 09: movie theater\n";
  "movie theater"
  >::: [
         ( "largest area for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 50 (largest_area sample) );
         ( "largest area for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 4749672288 (largest_area input) );
       ]
