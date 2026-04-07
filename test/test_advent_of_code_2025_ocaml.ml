open OUnit2

let tests =
  "all tests"
  >::: [
         ( "secret entrance a with sample" >:: fun _ ->
           assert_equal ~printer:string_of_int 3
             (Secret_entrance.password "../testdata/day01_sample.txt"
                Secret_entrance.stops_at_zero) );
         ( "secret entrance a with input" >:: fun _ ->
           assert_equal ~printer:string_of_int 964
             (Secret_entrance.password "../testdata/day01_input.txt"
                Secret_entrance.stops_at_zero) );
         ( "secret entrance b with sample" >:: fun _ ->
           assert_equal ~printer:string_of_int 6
             (Secret_entrance.password "../testdata/day01_sample.txt"
                Secret_entrance.passes_on_zero) );
         ( "secret entrance b with input" >:: fun _ ->
           assert_equal ~printer:string_of_int 964
             (Secret_entrance.password "../testdata/day01_input.txt"
                Secret_entrance.passes_on_zero) );
       ]

let _ = run_test_tt_main tests
