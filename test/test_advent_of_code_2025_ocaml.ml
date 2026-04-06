open OUnit2

let tests = 
    "all tests" >::: [
        "secret entrance with sample" >::
            (fun _ -> assert_equal ~printer:string_of_int 3 (Secret_entrance.actual_password "../testdata/day01_sample.txt")) ;
        "secret entrance with input" >::
            (fun _ -> assert_equal ~printer:string_of_int 964 (Secret_entrance.actual_password "../testdata/day01_input.txt")) ;
    ]

let _ =
    run_test_tt_main tests
