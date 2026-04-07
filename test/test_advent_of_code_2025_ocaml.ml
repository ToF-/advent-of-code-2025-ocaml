open OUnit2

let suite name =
  let password = Advent_of_code_2025.Secret_entrance.password in
  let stops_at_zero = Advent_of_code_2025.Secret_entrance.stops_at_zero in
  let passes_on_zero = Advent_of_code_2025.Secret_entrance.passes_on_zero in
  let sample = "../testdata/day01_sample.txt" in
  let input = "../testdata/day01_input.txt" in
  Printf.printf "%s\n" name;
  name
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

let _ = run_test_tt_main (suite "secret entrance")
