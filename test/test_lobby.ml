open OUnit2

let tests =
  let total_joltage = Advent_of_code_2025.Lobby.total_joltage in
  let sample = "../testdata/day03_sample.txt" in
  Printf.printf "day 03: lobby\n";
  "lobby"
  >::: [
         ( "total joltage for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 0 (total_joltage sample) );
       ]
