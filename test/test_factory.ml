open OUnit2

let tests =
  let option_a = false in
  let sample = "../testdata/day10_sample.txt" in
  let input = "../testdata/day10_input.txt" in
  let button_presses = Advent_of_code_2025.Factory.button_presses in
  let button_switches = Advent_of_code_2025.Factory.button_switches in
  let reduce = Advent_of_code_2025.Factory.reduce in
  let print_matrix m =
    Printf.printf "\n";
    m
    |> List.iter (fun row ->
        row |> List.iter (fun x -> Printf.printf "%d " x);
        Printf.printf "\n")
  in

  Printf.printf "day 10: factory\n";
  "factory"
  >::: [
         ( "fewer button presses for sample (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 7
             (button_presses sample option_a) );
         ( "fewer button presses for input (A)" >:: fun _ ->
           assert_equal ~printer:string_of_int 512
             (button_presses input option_a) );
         ( "switches command of a button" >:: fun _ ->
           assert_equal [ 0; 4; 5 ] (button_switches (1 + 16 + 32)) );
         ( "reducing a trivial matrix" >:: fun _ ->
           let initial = [ [ 1; 7 ] ] in
           let final = [ [ 1; 7 ] ] in
           assert_equal final (reduce initial) );
         ( "reducing a 2x3 matrix" >:: fun _ ->
           let initial = [ [ 0; 1; 7 ]; [ 1; 0; 2 ] ] in
           let final = [ [ 1; 0; 2 ]; [ 0; 1; 7 ] ] in
           let result = reduce initial in
           print_matrix result;
           assert_equal final result );
         ( "reducing a 3x4 matrix with pivot in last row" >:: fun _ ->
           let initial = [ [ 0; 0; 1; 2 ]; [ 0; 1; 1; 3 ]; [ 1; 0; 0; 4 ] ] in
           let final = [ [ 1; 0; 0; 4 ]; [ 0; 1; 1; 3 ]; [ 0; 0; 1; 2 ] ] in
           let result = reduce initial in
           print_matrix initial;
           Printf.printf "->";
           print_matrix result;
           assert_equal final result );
         ( "reducing a 3x4 matrix with non zero value in last row" >:: fun _ ->
           let initial = [ [ 0; 1; 0; 7 ]; [ 1; 0; 0; 4 ]; [ 1; 0; 1; 8 ] ] in
           let final = [ [ 1; 0; 0; 4 ]; [ 0; 1; 0; 7 ]; [ 0; 0; 1; 4 ] ] in
           let result = reduce initial in
           print_matrix initial;
           Printf.printf "->";
           print_matrix result;
           assert_equal final result );
         ( "reducing a 4x5 matrix with a negative pivot" >:: fun _ ->
           let initial =
             [
               [ 1; 0; 1; 1; 4 ];
               [ 0; 1; 1; 1; 5 ];
               [ 0; 1; 0; 1; 2 ];
               [ 0; 0; 0; 1; 3 ];
             ]
           in
           let final =
             [
               [ 1; 0; 1; 1; 4 ];
               [ 0; 1; 1; 1; 5 ];
               [ 0; 0; 1; 0; 3 ];
               [ 0; 0; 0; 1; 3 ];
             ]
           in
           let result = reduce initial in
           print_matrix initial;
           Printf.printf "->";
           print_matrix result;
           Printf.printf "\n";
           assert_equal final result );
         ( "reducing a 2x3 matrix already in good form" >:: fun _ ->
           let initial = [ [ 1; 0; 2 ]; [ 0; 1; 7 ] ] in
           let result = reduce initial in
           assert_equal initial result );
         ( "reducing a 4x5 matrix column after column" >:: fun _ ->
           let initial =
             [
               [ 1; 1; 1; 1; 4 ];
               [ 0; 0; 1; 1; 5 ];
               [ 0; 1; 1; 1; 3 ];
               [ 0; 0; 0; 1; 1 ];
             ]
           in
           let final =
             [
               [ 1; 1; 1; 1; 4 ];
               [ 0; 1; 1; 1; 3 ];
               [ 0; 0; 1; 1; 5 ];
               [ 0; 0; 0; 1; 1 ];
             ]
           in
           let result = reduce initial in
           print_matrix initial;
           Printf.printf "->";
           print_matrix result;
           assert_equal final result );
         ( "reducing a 4x7 matrix partially" >:: fun _ ->
           let initial =
             [
               [ 1; 1; 1; 1; 1; 0; 4 ];
               [ 0; 0; 1; 1; 1; 1; 5 ];
               [ 0; 1; 1; 1; 1; 0; 3 ];
               [ 0; 0; 0; 1; 1; 0; 1 ];
             ]
           in
           let final =
             [
               [ 1; 1; 1; 1; 1; 0; 4 ];
               [ 0; 1; 1; 1; 1; 0; 3 ];
               [ 0; 0; 1; 1; 1; 1; 5 ];
               [ 0; 0; 0; 1; 1; 0; 1 ];
             ]
           in
           let result = reduce initial in
           print_matrix initial;
           Printf.printf "->";
           print_matrix result;
           assert_equal final result );
       ]
