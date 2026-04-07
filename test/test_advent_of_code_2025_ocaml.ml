open OUnit2

let rec oracle_ticks_left pos delta =
  match delta with
  | 0 when pos == 0 -> 1
  | 0 when pos > 0 -> 0
  | x when pos == 0 -> 1 + oracle_ticks_left 99 (delta - 1)
  | x -> oracle_ticks_left (pos - 1) (delta - 1)

let rec oracle_ticks_right pos delta =
  match delta with
  | 0 when pos == 0 -> 1
  | 0 when pos > 0 -> 0
  | x when pos == 99 -> 1 + oracle_ticks_right 0 (delta - 1)
  | x -> oracle_ticks_right (pos + 1) (delta - 1)

let rec oracle ticks pos values =
  match values with
  | [] -> ticks
  | delta :: rest when delta < 0 ->
      oracle
        (ticks + oracle_ticks_left pos (-delta))
        (Secret_entrance.modulo (pos + delta) 100)
        rest
  | delta :: rest ->
      oracle
        (ticks + oracle_ticks_right pos delta)
        (Secret_entrance.modulo (pos + delta) 100)
        rest

let oracle_password file_name =
  let lines = Secret_entrance.read_lines file_name in
  let values = List.map Secret_entrance.dial lines in
  oracle 0 50 values

let tests =
  "all tests"
  >::: [
         ( "secret entrance a with sample" >:: fun _ ->
           assert_equal ~printer:string_of_int 3
             (Secret_entrance.actual_password_a "../testdata/day01_sample.txt")
         );
         ( "secret entrance a with input" >:: fun _ ->
           assert_equal ~printer:string_of_int 964
             (Secret_entrance.actual_password_a "../testdata/day01_input.txt")
         );
         ( "secret entrance b with sample" >:: fun _ ->
           assert_equal ~printer:string_of_int 6
             (Secret_entrance.actual_password_b "../testdata/day01_sample.txt")
         );
         ( "secret entrance b oracle with sample" >:: fun _ ->
           assert_equal ~printer:string_of_int 6
             (oracle_password "../testdata/day01_sample.txt") );
         ( "ticks for R1000" >:: fun _ ->
           assert_equal ~printer:string_of_int 10
             (Secret_entrance.ticks_b 0 50 [ 1000 ]) );
         ( "ticks for R0" >:: fun _ ->
           assert_equal ~printer:string_of_int 0
             (Secret_entrance.ticks_b 0 50 [ 0 ]) );
         (*        "secret entrance b with input" >::
            (fun _ -> assert_equal ~printer:string_of_int 6046 (Secret_entrance.actual_password_b "../testdata/day01_input.txt")) ; *)
       ]

let _ = run_test_tt_main tests
